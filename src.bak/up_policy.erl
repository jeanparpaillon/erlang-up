%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(up_policy).

-include("up.hrl").

-behaviour(gen_server).

-export([start_link/3,
	 next/1]).

%% Interface with monitor
-export([node_change/1]).

%% callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-callback init(Cluster :: pid(), Opts :: any()) -> {ok, State :: any()} | {error, any()}.
-callback next(State :: any()) -> {atom() | undefined, State :: any()}.
-callback node_change(State :: any()) -> {ok, State :: any()} | {error, any()}.

-record(state, {
	  mod :: atom(),
	  state :: any()
	 }).

-type opt() :: {policy, t()}.

-type t() :: round_robin
	   | random.

-export_type([opt/0, t/0]).

-spec start_link(#cluster{}, t(), term()) -> {ok, pid()} | {error, term()}.
start_link(Cluster, Policy, Opts) ->
    gen_server:start_link({local, Cluster#cluster.policy}, ?MODULE, [Cluster, Policy, Opts], []).


-spec next(#cluster{}) -> atom().
next(Cluster) ->
    gen_server:call(Cluster#cluster.policy, next).


-spec node_change(#cluster{}) -> ok.
node_change(Cluster) ->
    gen_server:cast(Cluster#cluster.policy, node_change).


%%
%% callbacks
%%
init([Cluster, Policy, Opts]) ->
    Mod = policy_to_mod(Policy),
    case Mod:init(Cluster, Opts) of
	{ok, PState} -> 
	    {ok, #state{mod=Mod, state=PState}};
	{error, Err} ->
	    {stop, Err}
    end.


handle_call(next, _From, #state{mod=Mod, state=State0}=S) ->
    {Node, State} = Mod:next(State0),
    {reply, Node, S#state{state=State}}.


handle_cast(node_change, #state{mod=Mod, state=State0}=S) -> 
    case Mod:node_change(State0) of
	{ok, State} -> {noreply, S#state{state=State}};
	{error, Err} -> {stop, Err, S}
    end.


handle_info(_Info, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%%
%%% Private
%%%
policy_to_mod(round_robin) -> ranch_ha_policy_round_robin;
policy_to_mod(random) -> ranch_ha_policy_random.
