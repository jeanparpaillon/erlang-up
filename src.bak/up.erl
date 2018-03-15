%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(up).

-include("up.hrl").

-behaviour(application).

-export([start_cluster/2]).

%% Application callbacks
-export([start/2, stop/1]).

-type opt() :: {refresh, integer()}
	     | {policy, ranch_ha_policy:t()}.

-export_type([opt/0]).

-spec start_cluster(Nodes :: [atom()], 
		    Opts :: [opt()]) ->
			   {ok, atom()} | {error, term()}.
start_cluster(Nodes, Opts) ->
    case up_sup:start_cluster(Nodes, Opts) of
	{error, {already_started, Pid}} -> {ok, Pid};
	{error, Err} -> {error, Err};
	{ok, Pid} -> {ok, Pid}
    end.

%% Application callbacks
start(_StartType, _StartArgs) ->
    up_sup:start_link().

stop(_State) ->
    ok.
