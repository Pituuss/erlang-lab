-module(pollution_test).
-author("Piotr Wojtyś").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

createMonitorTest() ->
  ?assertEqual({ok, #{stationsNames => #{}, stationsCords => #{}, measuredValues => #{}, id => 0}}, pollution:createMonitor()).

addStationTest() ->
  {ok, Monitor} = pollution:createMonitor(),

  Monitor0 = #{stationsNames => #{"Station1" => 0}, stationsCords => #{{1, 1} => 0}, measuredValues => #{0 => []}, id => 1},

  ?assertEqual(pollution:addStation(Monitor, "Station1", {1, 1}), {ok, Monitor0}),

  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addStation(Monitor1, "Station2", {2, 2}),

  ?assertEqual(maps:get("Station1", maps:get(stationsNames, Monitor1)), 0),

  ?assertEqual(
    #{stationsNames => #{"Station1" => 0, "Station2" => 1}
      , stationsCords => #{{1, 1} => 0, {2, 2} => 1}
      , measuredValues => #{0 => [], 1 => []}
      , id => 2}, Monitor2
  ),

  ?assertEqual(pollution:addStation(Monitor, 13, "totaly wrong cords"), {error, "Wrong arguments"}).

addValueTest() ->
  .


