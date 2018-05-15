%%%-------------------------------------------------------------------
%%% @author pituuss
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 1:52 PM

%%TODO create state table

%%%-------------------------------------------------------------------
-module(pollution_server_OTP).
-behavior(gen_server).

%% API
-export([
  start_link/0,
  addStation/2,
  crash/0,
  close/0,
  getMonitor/0,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getMaximumGradientStation/1
]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).

%% START %%
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% INTERFEJS KLIENT -> SERWER %%
addStation(Name, Cords) ->
  gen_server:call(?MODULE, {add_station, Name, Cords}).

addValue(ID, Date, Type, Value) ->
  gen_server:call(?MODULE, {add_value, ID, Date, Type, Value}).

removeValue(ID, Date, Type) ->
  gen_server:cast(?MODULE, {remove_value, ID, Date, Type}).

getOneValue(ID, Date, Type) ->
  gen_server:call(?MODULE, {get_one_value, ID, Date, Type}).

getStationMean(ID, Type) ->
  gen_server:call(?MODULE, {get_station_mean, ID, Type}).

getDailyMean(ID, Type) ->
  gen_server:call(?MODULE, {get_daily_mean, ID, Type}).

getMaximumGradientStation(Type) ->
  gen_server:call(?MODULE, {get_max_grad_station, Type}).

getMonitor() ->
  gen_server:call(?MODULE, get_state).

crash() ->
  gen_server:cast(?MODULE, crash).

close() ->
  gen_server:call(?MODULE, terminate).

init(_args) ->
  pollution:createMonitor().

%% OBSŁUGA WIADOMOŚCI %%
handle_call(get_state, _Form, Monitor) ->
  {reply, Monitor, Monitor};
handle_call({add_station, Name, Cords}, _From, Monitor) ->
  case pollution:addStation(Monitor, Name, Cords) of
    {ok, NMonitor} ->
      {reply, "station added succesfuly", NMonitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end;
handle_call({add_value, ID, Date, Type, Value}, _Form, Monitor) ->
  case pollution:addValue(Monitor, ID, Date, Type, Value) of
    {ok, NMonitor} ->
      {reply, "value added succesfuly", NMonitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end;
handle_call({get_one_val, ID, Date, Type}, _From, Monitor) ->
  case pollution:getOneValue(Monitor, ID, Date, Type) of
    {ok, Val} ->
      {reply, Val, Monitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end;
handle_call({get_station_mean, ID, Type}, _Form, Monitor) ->
  case pollution:getStationMean(Monitor, ID, Type) of
    {ok, Mean} ->
      {reply, Mean, Monitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end;
handle_call({get_daily_mean, Day, Type}, _From, Monitor) ->
  case pollution:getDailyMean(Monitor, Day, Type) of
    {ok, Mean} ->
      {reply, Mean, Monitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end;
handle_call({get_max_grad_station, Type}, _From, Monitor) ->
  case pollution:getMaximumGradientStation(Monitor, Type) of
    {ok, Mean} ->
      {reply, Mean, Monitor};
    {error, Cause} ->
      {reply, Cause, Monitor}
  end.

handle_cast({remove_value, ID, Date, Type}, Monitor) ->
  {ok, NMonitor} = pollution:removeValue(Monitor, ID, Date, Type),
  {noreply, NMonitor};
handle_cast(crash, Monitor) ->
  1 = 0,
  {noreply, Monitor}.

terminate(normal, Monitor) -> io:format("The monitor was: ~B~nBye.~n", [Monitor]), ok.