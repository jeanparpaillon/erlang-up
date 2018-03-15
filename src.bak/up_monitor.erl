%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(up_monitor).

-include("up.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/2,
	 'master?'/1,
	 alive/1,
	 all/1]).

%% gen_statem callbacks
-export([init/1, 
	 callback_mode/0,
	 code_change/4,
	 terminate/3]).

%% State functions
-export([master/3,
	 slave/3]).

-define(DELAY, 2000).
-define(NODES_TID, up_monitor_nodes).


-spec start_link([node()], [ranch_ha:opt()]) -> {ok, pid()} | {error, term()}.
start_link(Nodes, Opts) ->
    Args = [Nodes, Opts],
    gen_statem:start_link(?MODULE, 
			  #{ nodes => Nodes,
			     refresh => proplists:get_value(refresh, Opts, ?DELAY),
			     master_up => proplists:get_value(master_up, Opts, undefined),
			     master_down => proplists:get_value(master_down, Opts, undefined),
			     args => Args }, []).


-spec 'master?'(pid()) -> master | slave.
'master?'(Cluster) ->
    gen_statem:call(Cluster, 'master?').


-spec alive(pid()) -> [atom()].
alive(Cluster) ->
    gen_statem:call(Cluster, alive).


-spec all(pid()) -> [{atom(), boolean()}].
all(Cluster) ->
    gen_statem:call(Cluster, all).

%%%
%%% Private
%%%
callback_mode() ->
    state_functions.


init(#{ nodes := NodesList }=Opts) ->
    ?debug("Start monitoring nodes: ~p", [NodesList]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    T_Nodes = init_table(NodesList),
    ok = connect(T_Nodes),
    Refresh = maps:get(refresh, Opts),
    Data = Opts#{ nodes := T_Nodes },
    {ok, _} = timer:send_after(Refresh, refresh),
    init_state('status?'(node(), T_Nodes), Data).


terminate(_Reason, _Data, _State) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


master(info, {nodeup, Node, _Infos}, #{ nodes := T_Nodes }=Data) ->
    Priority = priority(Node, T_Nodes),
    ok = nodeup(Node, Data),
    case priority(node(), T_Nodes) of
	MyPriority when Priority < MyPriority ->
	    case master_down(Data) of
		ok -> {next_state, slave, Data};
		{error, Err} -> {stop, Err}
	    end;
	_ ->
	    ?debug("New node up: ~s", [Node]),
	    {keep_state, Data}
    end;

master(info, {nodedown, Node, _Infos}, Data) ->
    ok = nodedown(Node, Data),
    {keep_state, Data};

master({call, From}, 'master?', Data) ->
    {keep_state, Data, [{reply, From, master}]};

master(Type, Other, Data) ->
    handle_event(Type, Other, Data).


slave(info, {nodeup, Node, _Infos}, Data) ->
    ok = nodeup(Node, Data),
    {next_state, slave, Data};

slave(info, {nodedown, Node, _Infos}, #{ nodes := T_Nodes }=Data) ->
    Priority = priority(Node, T_Nodes),
    ok = nodedown(Node, Data),
    case priority(node(), T_Nodes) of
	MyPriority when Priority > MyPriority ->
	    ?debug("SLAVE node down ~s", [Node]),
	    {next_state, slave, Data};
	MyPriority when Priority < MyPriority ->
	    case 'status?'(node(), T_Nodes) of
		master ->
		    case master_up(Data) of
			ok -> {next_state, master, Data};
			{error, Err} -> {stop, Err}
		    end;
		slave ->
		    {next_state, slave, Data}
	    end
    end;

slave({call, From}, 'master?', Data) ->
    {keep_state, Data, [{reply, From, slave}]};

slave(Type, Other, Data) ->
    handle_event(Type, Other, Data).


%%%===================================================================
%%% Internal functions
%%%===================================================================
init_table(NodesList) ->
    T_Nodes = ets:new(?NODES_TID, [{read_concurrency, true}, ordered_set]),
    _ = lists:foldl(fun (Node, Prio) ->
			    true = ets:insert(T_Nodes, {Prio, Node, node() == Node}),
			    Prio+1
		    end, 0, NodesList),
    T_Nodes.



init_state(master, Data) ->
    case master_up(Data) of
	ok -> {ok, master, Data};
	{error, Err} -> {stop, Err}
    end;

init_state(slave, Data) ->
    {ok, slave, Data}.


%% If master fails, mutiny !!! (kill the master)
master_up(#{ master_up := Fun }) ->
    ?info("Set MASTER mode", []),
    Ret = try Fun(Cluster) of
	     ok -> ok;
	     {error, Err0} -> {error, Err0}
	 catch Cls:Err1 ->
		 {error, {Cls, Err1}}
	 end,
    case Ret of
	ok -> ok;
	{error, Err} ->
	    ?error("Sorry but I have to kill this bad master: ~p", [Err]),
	    erlang:halt(1)
    end.	     


master_down(#{ master_down := Fun, cluster := Cluster }) ->
    ?info("Set SLAVE mode", []),
    try Fun(Cluster) of
	ok -> ok;
	{error, Err} -> 
	    ?debug("Not master anymore, ignore error: ~p", [Err]),
	    ok
    catch Cls:Err ->
	    ?debug("Not master anymore, ignore error: ~p:~p", [Cls, Err]),
	    ok
    end.	    	    


priority(Node, Tid) -> 
    [ [Prio] ] = ets:match(Tid, {'$1', Node, '_'}),
    Prio.


nodeup(Node, #{ nodes := T_Nodes}=Data) -> 
    true = rpc:cast(Node, ranch_ha, start_cluster, maps:get(args, Data)),
    [[Prio]] = ets:match(T_Nodes, {'$1', Node, '_'}),
    true = ets:insert(T_Nodes, {Prio, Node, true}),
    ranch_ha_policy:node_change(maps:get(cluster, Data)).
	

nodedown(Node, #{ nodes := T_Nodes }=Data) -> 
    [[Prio]] = ets:match(T_Nodes, {'$1', Node, '_'}),
    true = ets:insert(T_Nodes, {Prio, Node, false}),
    ranch_ha_policy:node_change(maps:get(cluster, Data)).


'status?'(Node, Nodes) -> 
    'status?'(Node, ets:first(Nodes), Nodes).


'status?'(_Node, '$end_of_table', _) -> slave;
%% No node of higher priority is alive -> master
'status?'(Node, Prio, Nodes) -> 
    case ets:lookup(Nodes, Prio) of
	[{_, Node, true}] -> master;
	[{_, Node, false}] -> slave;
	[{_, _OtherNode, true}] -> slave;
	[{_, _OtherNode, false}] -> 'status?'(Node, ets:next(Nodes, Prio), Nodes)
    end.


alive_(T_Nodes) ->
    lists:map(fun ([Node]) -> Node end, ets:match(T_Nodes, {'_', '$1', true})).


connect(T_Nodes) -> 
    true = ets:insert(T_Nodes, lists:map(fun ({Prio, Node, false}) ->
						 {Prio, Node, net_kernel:connect_node(Node)}
					 end, ets:match_object(T_Nodes, {'_', '_', false}))),
    ok.


handle_event({call, From}, alive, #{ nodes := T_Nodes }=Data) ->
    {keep_state, Data, [{reply, From, alive_(T_Nodes)}]};

handle_event(info, refresh, #{ nodes := T_Nodes, refresh := Delay }=Data) ->
    ok = connect(T_Nodes),
    {ok, _} = timer:send_after(Delay, refresh),
    {keep_state, Data}.
