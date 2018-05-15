%%%-------------------------------------------------------------------
%%% @author pituuss
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 1:24 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("pituuss").

%% API

%%% Client
-export([
  start/0,
  stop/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getMaximumGradientStation/1,
  crash/0,
  start_link/0]).

%%% Server
-export([
  init/0,
  server/1
]).

start() ->
  register(pollutionServer, spawn(?MODULE, init, [])).

start_link() ->
  register(pollutionServer, spawn_link(?MODULE, init, [])).

stop() ->
  pollutionServer ! shut_down.

init() ->
  {ok, Monitor} = pollution:createMonitor(),
  server(Monitor).

server(Monitor) ->
  receive
    {crash} -> 1 = 0;
    {shut_down, Pid} -> Pid ! {reply, ok};
    {request, add_station, Pid, Name, Cords} ->
      NewMonitor = pollution:addStation(Monitor, Name, Cords),
%%      io:format("~p~n",[NewMonitor]),
      case NewMonitor of
        {ok, NMonitor} ->
          Pid ! {reply, ok},
          pollution_server:server(NMonitor);
        {error, Cause} ->
          Pid ! {reply, Cause},
          pollution_server:server(Monitor)
      end;
    {request, add_value, Pid, ID, Date, Type, Value} ->
      NewMonitor = pollution:addValue(Monitor, ID, Date, Type, Value),
      case NewMonitor of
        {ok, NMonitor} ->
          Pid ! {reply, ok},
          pollution_server:server(NMonitor);
        {error, Cause} ->
          Pid ! {reply, Cause},
          pollution_server:server(Monitor)
      end;
    {request, remove_value, Pid, ID, Date, Type} ->
      NewMonitor = pollution:removeValue(Monitor, ID, Date, Type),
      case NewMonitor of
        {ok, NMonitor} ->
          Pid ! {reply, ok},
          pollution_server:server(NMonitor);
        {error, Cause} ->
          Pid ! {reply, Cause},
          pollution_server:server(Monitor)
      end;
    {request, get_one_val, Pid, ID, Date, Type} ->
      {ok, Val} = pollution:getOneValue(Monitor, ID, Date, Type),
      Pid ! {reply, Val},
      pollution_server:server({ok, Monitor});
    {request, get_station_mean, Pid, ID, Type} ->
      {ok, Val} = pollution:getStationMean(Monitor, ID, Type),
      Pid ! {reply, Val},
      pollution_server:server({ok, Monitor});
    {request, get_daily_mean, Pid, Day, Type} ->
      {ok, Val} = pollution:getDailyMean(Monitor, Day, Type),
      Pid ! {reply, Val},
      pollution_server:server({ok, Monitor});
    {request, get_max_grad, Pid, Type} ->
      {ok, Val} = pollution:getMaximumGradientStation(Monitor, Type),
      Pid ! {reply, Val},
      pollution_server:server({ok, Monitor})
  end.

%% client

crash() ->
  pollutionServer ! {crash}.

addStation(Name, Cords) ->
  pollutionServer ! {request, add_station, self(), Name, Cords},
  receive
    {reply, Reply} -> Reply
  end.

addValue(ID, Date, Type, Value) ->
  pollutionServer ! {request, add_value, self(), ID, Date, Type, Value},
  receive
    {reply, Reply} -> Reply
  end.

removeValue(ID, Date, Type) ->
  pollutionServer ! {request, remove_station, self(), ID, Date, Type},
  receive
    {reply, Reply} -> Reply
  end.

getOneValue(ID, Date, Type) ->
  pollutionServer ! {request, get_one_val, self(), ID, Date, Type},
  receive
    {reply, Reply} -> Reply
  end.

getStationMean(ID, Type) ->
  pollutionServer ! {request, get_station_mean, self(), ID, Type},
  receive
    {reply, Reply} -> Reply
  end.

getDailyMean(Day, Type) ->
  pollutionServer ! {request, get_daily_mean, self(), Day, Type},
  receive
    {reply, Reply} -> Reply
  end.

getMaximumGradientStation(Type) ->
  pollutionServer ! {request, get_max_grad, self(), Type},
  receive
    {reply, Reply} -> Reply
  end.