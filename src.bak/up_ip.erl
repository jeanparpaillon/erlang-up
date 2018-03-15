%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2016 Jean Parpaillon.
-module(ranch_ha_ip).

-include("ranch_ha.hrl").

%% API
-export([addifaddr/3,
	 delifaddr/3]).

-spec addifaddr(Netif :: string(), Ip :: inet:ip_address(), Netmask :: inet:ip_address()) ->
		       ok | {error, term()}.
addifaddr(Netif, Ip, Netmask) when is_list(Netif) ->
    try cmd("add", Ip, Netmask, Netif)
    catch throw:Err -> {error, Err}
    end.


-spec delifaddr(Netif :: string(), Ip :: inet:ntoa(), Netmask :: inet:ntoa()) ->
		       ok | {error, term()}.
delifaddr(Netif, Ip, Netmask) ->
    try cmd("del", Ip, Netmask, Netif)
    catch throw:Err -> {error, Err}
    end.


%%%
%%% Internal
%%%
cmd(Op, Ip, Netmask, Netif) ->
    IpStr = case inet:ntoa(Ip) of
		{error, Err} -> throw(Err);
		S -> S
	    end,
    NetmaskStr = case inet:ntoa(Netmask) of
		     {error, Err1} -> throw(Err1);
		     S1 -> S1
		 end,
    Cmd = io_lib:format("LANG=C /sbin/ip address ~s ~s/~s dev ~s 2>&1", [Op, IpStr, NetmaskStr, Netif]),
    case os:cmd(Cmd) of
	[] -> ok;
	"RTNETLINK answers: Cannot assign requested address" ++ _ ->
	    {error, enoent};
	"RTNETLINK answers: Operation not permitted" ++ _ ->
	    ?error("Check you have capability CAP_NET_ADMIN", []),
	    {error, eperm};
	"RTNETLINK answers: Operation not supported" ++ _ ->
	    {error, eafnosupport};
	"RTNETLINK answers: File exists" ++ _ ->
	    %{error, eexist};
	    ok;
	"Cannot find device" ++ _ ->
	    {error, enodev};
	"Error: inet prefix is expected rather than" ++ _ ->
	    {error, einval};
	Other ->
	    {error, Other}
    end.


