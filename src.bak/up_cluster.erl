%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(up_cluster).

-include("up.hrl").

-behaviour(supervisor).

%% API
-export([start_link/2,
	 start_lb/2]).

%% Supervisor callbacks
-export([init/1]).


-spec start_link(Nodes :: [node()], term()) -> {ok, pid()} | {error, term()}.
start_link(Nodes, Opts) ->
    supervisor:start_link(?MODULE, [Nodes, Opts]).


-spec start_lb(Cluster :: pid(), Opts :: [up_policy:opt()]) -> {ok, pid()} | {error, term()}.
start_lb(Cluster, Opts) ->
    PolicySpec = #{ id => make_ref(),
		    start => {up_policy, start_link, [Cluster, Opts]}},
    supervisor:start_child(Cluster, PolicySpec).

%%%
%%% Private
%%%
init([Nodes, Opts]) ->
    SupFlags = #{ strategy => one_for_all,
		  intensity => 1,
		  period => 5 },

    Monitor = #{ id => make_ref(),
		 start => {up_monitor, start_link, [Nodes, Opts]},
		 type => worker },
    {ok, {SupFlags, [Monitor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
