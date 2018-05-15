%%%-------------------------------------------------------------------
%%% @author pituuss
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 2:10 PM
%%%-------------------------------------------------------------------
-module(supervisor_OTP).
-author("pituuss").
-behavior(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, supervisor_OTP}, ?MODULE, []).

init(_InitVal) ->
  {ok, {
    {one_for_all, 2, 3},
    [
      {pollution_server_OTP,
        {pollution_server_OTP, start_link, []},
        permanent, brutal_kill, worker, [polution_server_OTP]}
    ]}
  }.


