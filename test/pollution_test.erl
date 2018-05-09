-module(pollution_test).
-author("Piotr Wojtyś").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


create_monitor_test() ->
  ?assertEqual({ok, #{stationsNames => #{}, stationsCords => #{}, measuredValues => #{}, id => 0}}, pollution:createMonitor()).

add_station_test() ->
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

  ?assertEqual({error, "Station already exisists"}, pollution:addStation(Monitor2, "Station2", {3, 3})),
  ?assertEqual({error, "Station already exisists"}, pollution:addStation(Monitor2, "Station3", {2, 2})),

  ?assertEqual({error, "Wrong arguments"}, pollution:addStation(Monitor, 13, "totaly wrong cords")).

add_value_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  Monitor0 = #{
    stationsNames => #{"Station1" => 0}
    , stationsCords => #{{1, 1} => 0}
    , measuredValues => #{
      0 => [
        #{type => "TEMP"
          , value => 30.0
          , date => {{1, 1, 1}, {1, 1, 1}}}
      ]
    }
    , id => 1},
  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),

  ?assertEqual({ok, Monitor0}, pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),
  ?assertEqual({ok, Monitor0}, pollution:addValue(Monitor1, {1, 1}, {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),

  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  ?assertEqual({error, "Measure already taken"}, pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),
  ?assertEqual({error, "Measure already taken"}, pollution:addValue(Monitor2, {1, 1}, {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),
  ?assertEqual({error, "Measure already taken"}, pollution:addValue(Monitor2, {1, 1}, {{1, 1, 1}, {1, 1, 1}}, "TEMP", 20.0)),
  ?assertEqual({error, "Station doesn't exist"}, pollution:addValue(Monitor2, "Station3", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),
  ?assertEqual({error, "Station doesn't exist"}, pollution:addValue(Monitor2, {10, 10}, {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0)),

  Monitor01 = #{
    stationsNames => #{"Station1" => 0}
    , stationsCords => #{{1, 1} => 0}
    , measuredValues =>#{
      0 => [
        #{type => "TEMP", value => 30.0, date => {{1, 1, 1}, {1, 1, 1}}}
        #{type => "TEMP", value => 30.0, date => {{1, 1, 1}, {1, 1, 2}}}
      ]
    }
    , id => 1
  },
  Monitor02 = #{
    stationsNames => #{"Station1" => 0}
    , stationsCords => #{{1, 1} => 0}
    , measuredValues =>#{
      0 => [
        #{type => "TEMP", value => 30.0, date => {{1, 1, 1}, {1, 1, 1}}}
        #{type => "OTHER", value => 30.0, date => {{1, 1, 1}, {1, 1, 1}}}
      ]
    }
    , id => 1
  },


  ?assertEqual({ok, Monitor01}, pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 30.0)),
  ?assertEqual({ok, Monitor02}, pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "OTHER", 30.0)),
  ?assertEqual({error, "Wrong arguments"}, pollution:addValue(Monitor1, 13, "totaly wrong time", 1, 1)).

remove_value_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor3} = pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 30.0),

  ?assertEqual({ok, Monitor3}, pollution:removeValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP2")),
  ?assertEqual({ok, Monitor3}, pollution:removeValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 3}}, "TEMP")),
  Monitor01 = #{
    stationsNames => #{"Station1" => 0}
    , stationsCords => #{{1, 1} => 0}
    , measuredValues =>#{
      0 => [
        #{type => "TEMP", value => 30.0, date => {{1, 1, 1}, {1, 1, 1}}}
      ]
    }
    , id => 1
  },
  ?assertEqual({ok, Monitor01}, pollution:removeValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({ok, Monitor01}, pollution:removeValue(Monitor3, {1, 1}, {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({ok, Monitor01}, pollution:removeValue(Monitor3, {1, 1}, {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({error, "Wrong arguments"}, pollution:removeValue(Monitor3, 13, "totaly wrong time", 1)).

get_one_value_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor3} = pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 30.0),

  ?assertEqual({error, "Station doesn't exist"}, pollution:getOneValue(Monitor3, "Station2", {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({error, "Station doesn't exist"}, pollution:getOneValue(Monitor3, {2, 2}, {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({ok, 30.0}, pollution:getOneValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({ok, 30.0}, pollution:getOneValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP")),
  ?assertEqual({ok, nothing}, pollution:getOneValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 2}}, "OTHER")),
  ?assertEqual({ok, nothing}, pollution:getOneValue(Monitor3, "Station1", {{1, 1, 1}, {1, 1, 3}}, "TEMP")),
  ?assertEqual({error, "Wrong arguments"}, pollution:getOneValue(Monitor3, 13, "totaly wrong time", 1)).

get_station_mean_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor3} = pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 3.0),
  ?assertEqual({error, "Station doesn't exist"}, pollution:getStationMean(Monitor3, "Station2", "TEMP")),
  ?assertEqual({error, "Station doesn't exist"}, pollution:getStationMean(Monitor3, {2, 2}, "TEMP")),
  ?assertEqual({ok, 33.0 / 2.0}, pollution:getStationMean(Monitor3, "Station1", "TEMP")),
  ?assertEqual({ok, 33.0 / 2.0}, pollution:getStationMean(Monitor3, {1, 1}, "TEMP")),
  ?assertEqual({ok, nothing}, pollution:getStationMean(Monitor3, "Station1", "OTHER")),
  ?assertEqual({ok, nothing}, pollution:getStationMean(Monitor3, {1, 1}, "OTHER")),
  ?assertEqual({error, "Wrong arguments"}, pollution:getStationMean(Monitor3, 13, 3)).

get_daily_mean_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor1} = pollution:addStation(Monitor, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor3} = pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 3.0),

  ?assertEqual({ok, 33.0 / 2.0}, pollution:getDailyMean(Monitor3, 1, "TEMP")),
  ?assertEqual({ok, nothing}, pollution:getDailyMean(Monitor3, 1, "OTHER")),
  ?assertEqual({error, "Wrong arguments"}, pollution:getDailyMean(Monitor3, "ALA", 3)).

get_maximum_gradient_test() ->
  {ok, Monitor} = pollution:createMonitor(),
  {ok, Monitor00} = pollution:addStation(Monitor, "Station3", {3, 3}),
  {ok, Monitor01} = pollution:addValue(Monitor00, "Station3", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor1} = pollution:addStation(Monitor01, "Station1", {1, 1}),
  {ok, Monitor2} = pollution:addValue(Monitor1, "Station1", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor3} = pollution:addValue(Monitor2, "Station1", {{1, 1, 1}, {1, 1, 2}}, "TEMP", 3.0),
  {ok, Monitor4} = pollution:addStation(Monitor3, "Station2", {2, 2}),
  {ok, Monitor5} = pollution:addValue(Monitor4, "Station2", {{1, 1, 1}, {1, 1, 1}}, "TEMP", 30.0),
  {ok, Monitor6} = pollution:addValue(Monitor5, "Station2", {{1, 1, 1}, {1, 1, 2}}, "TEMP", -10.0),

  ?assertEqual({error, "Wrong arguments"}, pollution:getMaximumGradientStation(Monitor3, 3)),
  ?assertEqual({ok, {"Station3", 0}}, pollution:getMaximumGradientStation(Monitor01, "TEMP")),
  ?assertEqual({ok, {"Station2", 40.0}}, pollution:getMaximumGradientStation(Monitor6, "TEMP")),
  ?assertEqual({ok, {"Station1", 33.0}}, pollution:getMaximumGradientStation(Monitor3, "TEMP")).
