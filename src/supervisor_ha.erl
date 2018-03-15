%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Distributed supervisor ensuring children are running on one node of
%%% a group of nodes.
%%%
%%% @end
%%% Created : 16 Nov 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(supervisor_ha).

-behaviour(gen_server).

-type ha_opt() :: {nodes, [node()]}.

-export([start_link/4,
	 start_child/2, restart_child/2,
	 delete_child/2, terminate_child/2,
	 which_children/1, count_children/1,
	 check_childspecs/1]).

%% Callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {
	  pid             :: pid(),
	  node            :: node(),
	  name            :: atom(),
	  module          :: atom(),
	  args            :: term()
	 }).

-spec start_link(Name :: atom(), Module :: atom(), Args :: any(), HaOpts :: [ha_opt()]) -> supervisor:startlink_ret().
start_link(Name, Module, Args, HaOpts) ->
    ok = pg2:create(Name),
    HaArgs = [Name, Module, Args, HaOpts],
    case gen_server:start_link({local, Name}, ?MODULE, HaArgs, []) of
	{ok, Pid} ->
	    ok = pg2:join(Name, Pid),
	    {ok, Pid};
	ignore -> ignore;
	{error, Err} -> {error, Err}
    end.


start_child(Ref, ChildSpec) -> call(Ref, {start_child, ChildSpec}).

restart_child(Ref, Name) -> call(Ref, {restart_child, Name}).

delete_child(Ref, Name) -> call(Ref, {delete_child, Name}).

terminate_child(Ref, Name) -> call(Ref, {terminate_child, Ref, Name}).

which_children(Ref) -> call(Ref, which_children).

count_children(Ref) -> call(Ref, count_children).

check_childspecs(ChildSpecs) -> supervisor:check_childspecs(ChildSpecs).

%%
%% Callbacks
%%
init([Name, Module, Args, HaOpts]) ->
    ok = monitor_nodes(proplists:get_value(nodes, HaOpts, [])),
    _ = process_flag(trap_exit, true),
    S0 = #state{name=Name, module=Module, args=Args},
    start_sup(S0).


handle_call({start_child, ChildSpec}, _From, S) ->
    {reply, ok, S}.


handle_cast(_Cast, S) ->
    {noreply, S}.


handle_info({'EXIT', Sup, _Reason}, #state{pid=Sup}=S0) ->
    case start_sup(S0) of
	{ok, S} -> {noreply, S};
	{error, Err} -> {stop, Err, S0}
    end.


terminate(_Reason, _S) ->
    ok.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%
%% Private
%%
start_sup(#state{name=Name, module=Module, args=Args}=S0) ->
    case supervisor_local:start_link({global, Name}, Module, Args) of
	{ok, SupPid} -> 
	    {ok, S0#state{pid=SupPid, node=node()}};
	ignore -> 
	    {ok, S0#state{pid=undefined, node=node()}};
	{error, {already_started, SupPid}} ->
	    true = link(SupPid),
	    {ok, S0#state{pid=SupPid, node=node(SupPid)}};
	{error, Err} -> 
	    {error, Err}
    end.
    

monitor_nodes([]) ->
    ok;

monitor_nodes(Nodes) ->
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    lists:foreach(fun (Node) ->
			  _ = net_kernel:connect_node(Node)
		  end, Nodes).


call(Name, Call) ->
    case pg2:get_members(Name) of
	{error, Err} -> {error, Err};
	Pids ->
	    lists:foldl(fun (Pid, Acc) ->
				call_one(Pid, Call, Name, Acc)
			end, ok, Pids)
    end.


call_one(Pid, Call, Name, Acc) ->
    try gen_server:call(Pid, Call) of
	ignore ->
	    %% Got call, but not holding supervisor right now
	    Acc;
	Ret ->
	    %% Got call and holding supervisor: ret from supervisor
	    Ret
    catch 
	exit:{noproc, _} ->
	    %% Process does not run anymore, remove from group
	    ok = pg2:leave(Name, Pid),
	    Acc
    end.
