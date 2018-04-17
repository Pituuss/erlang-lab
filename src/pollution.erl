-module(pollution).
-author("Piotr Wojtyś").

%% API
-export([
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumGradientStation/2,
  unpack/1
]).

unpack({_, Monitor}) when is_map(Monitor) ->
  Monitor.

createMonitor() ->
  {ok, #{stationsNames => #{}, stationsCords => #{}, measuredValues => #{}, id => 0}}.

addStation(Monitor, Name, Cords) when is_tuple(Cords), is_list(Name) ->
  case not (maps:is_key(Name, gStationsNames(Monitor)) or maps:is_key(Cords, gStationsCords(Monitor))) of
    true -> {ok, Monitor#{
      stationsNames := (gStationsNames(Monitor))#{Name => gID(Monitor)},
      stationsCords := (gStationsCords(Monitor))#{Cords => gID(Monitor)},
      measuredValues := (gMeasuredValues(Monitor))#{gID(Monitor) => []},
      id => gID(Monitor) + 1
    }};
    false -> {error, "Station already exisists"}
  end;
addStation(_, _, _) -> {error, "Wrong arguments"}.

addValue(Monitor, StationID, Date, Type, Value) when is_tuple(StationID), is_tuple(Date), is_list(Type), is_float(Value) ->
  case maps:find(StationID, gStationsCords(Monitor)) of
    {ok, ID} ->
      addValue_(Monitor, ID, Date, Type, Value);
    error ->
      {error, "Station doesn't exist"}
  end;

addValue(Monitor, StationID, Date, Type, Value) when is_list(StationID), is_list(Type), is_float(Value) ->
  case maps:find(StationID, gStationsNames(Monitor)) of
    {ok, ID} ->
      addValue_(Monitor, ID, Date, Type, Value);
    error ->
      {error, "Station doesn't exist"}
  end;
addValue(_, _, _, _, _) -> {error, "Wrong arguments"}.


addValue_(Monitor, ID, Date, Type, Value) ->
  case not lists:foldl(fun({X, Y}, ACC) ->
    ({X, Y} =:= {Date, Type}) or ACC end, false, [{X, Y} || X <- gListOfDates(ID, Monitor), Y <- gListOfTypes(ID, Monitor)]) of
    false ->
      {error, "Measure already taken"};
    true ->
      {ok, Monitor#{
        measuredValues := (gMeasuredValues(Monitor))#{
          ID => [#{type => Type, value => Value, date => Date} | maps:get(ID, gMeasuredValues(Monitor))]
        }
      }}
  end.

removeValue(Monitor, StationID, Date, Type) when is_list(StationID), is_tuple(Date), is_list(Type) ->
  case maps:find(StationID, gStationsNames(Monitor)) of
    {ok, ID} -> {ok, Monitor#{
      measuredValues := (gMeasuredValues(Monitor))#{
        ID := [X || X <- maps:get(ID, gMeasuredValues(Monitor)), {gMeasureType(X), gMeasureDate(X)} =/= {Type, Date}]
      }
    }};
    error -> {ok, Monitor}
  end;

removeValue(Monitor, StationID, Date, Type) when is_tuple(StationID), is_tuple(Date), is_list(Type) ->
  case maps:find(StationID, gStationsCords(Monitor)) of
    {ok, ID} ->
      {ok, Monitor#{
        measuredValues := (gMeasuredValues(Monitor))#{
          ID := [X || X <- maps:get(ID, gMeasuredValues(Monitor)), {gMeasureType(X), gMeasureDate(X)} =/= {Type, Date}]
        }
      }};
    error -> {ok, Monitor}
  end;

removeValue(_, _, _, _) -> {error, "Wrong arguments"}.

getOneValue(Monitor, StationID, Date, Type) when is_tuple(StationID), is_tuple(Date), is_list(Type) ->
  case maps:find(StationID, gStationsNames(Monitor)) of
    {ok, ID} ->
      case [X || X <- maps:get(ID, gMeasuredValues(Monitor)), {gMeasureType(X), gMeasureDate(X)} =:= {Type, Date}] of
        [H | _] -> {ok, gMeasureValue(H)};
        _ -> {ok, nothing}
      end;
    error -> {error, "Station doesn't exist"}
  end;

getOneValue(Monitor, StationID, Date, Type) when is_list(StationID), is_tuple(Date), is_list(Type) ->
  case maps:find(StationID, gStationsNames(Monitor)) of
    {ok, ID} ->
      case [X || X <- maps:get(ID, gMeasuredValues(Monitor)), {gMeasureType(X), gMeasureDate(X)} =:= {Type, Date}] of
        [H | _] -> {ok, gMeasureValue(H)};
        _ -> {ok, nothing}
      end;
    error -> {error, "Station doesn't exist"}
  end;

getOneValue(_, _, _, _) -> {error, "Wrong arguments"}.

getStationMean(Monitor, StationID, Type) when is_list(StationID), is_list(Type) ->
  case maps:find(StationID, gStationsNames(Monitor)) of
    {ok, ID} ->
      {ok, calcMean(lists:filter(fun(X) -> gMeasureType(X) =:= Type end, maps:get(ID, gMeasuredValues(Monitor))))};
    error -> {error, "Station doesn't exist"}
  end;

getStationMean(Monitor, StationID, Type) when is_tuple(StationID), is_list(Type) ->
  case maps:find(StationID, gStationsCords(Monitor)) of
    {ok, ID} ->
      {ok, calcMean(lists:filter(fun(X) -> gMeasureType(X) =:= Type end, maps:get(ID, gMeasuredValues(Monitor))))};
    error -> {error, "Station doesn't exist"}
  end;

getStationMean(_, _, _) -> {error, "Wrong arguments"}.

getDailyMean(Monitor, Day, Type) when is_integer(Day), is_list(Type) ->
  {ok, calcMean(
    lists:filter(fun(Y) ->
      {gDay(gMeasureDate(Y)), gMeasureType(Y)} =:= {Day, Type} end,
      lists:flatten(
        [X || {_, X} <- maps:to_list(gMeasuredValues(Monitor))])
    ))};

getDailyMean(_, _, _) -> {error, "Wrong arguments"}.

getMaximumGradientStation(Monitor, Type) when is_list(Type) ->
  {ok, getMax([{Name, calcGradient(lists:filter(fun(Y) ->
    gMeasureType(Y) =:= Type end, maps:get(X, gMeasuredValues(Monitor))))} || {Name, X} <- maps:to_list(gStationsNames(Monitor))])};

getMaximumGradientStation(_, _) -> {error, "Wrong arguments"}.

%% Sub Functions

gStationsNames(Monitor) ->
  maps:get(stationsNames, Monitor).

gStationsCords(Monitor) ->
  maps:get(stationsCords, Monitor).

gMeasuredValues(Monitor) ->
  maps:get(measuredValues, Monitor).

gID(Monitor) ->
  maps:get(id, Monitor).

gMeasureType(Measure) ->
  maps:get(type, Measure).

gMeasureValue(Measure) when is_map(Measure) ->
  maps:get(value, Measure);
gMeasureValue(_) -> nothing.

gMeasureDate(Measure) ->
  maps:get(date, Measure).

gListOfDates(ID, Monitor) ->
  lists:map(fun gMeasureDate/1, maps:get(ID, gMeasuredValues(Monitor))).

gListOfTypes(ID, Monitor) ->
  lists:map(fun gMeasureType/1, maps:get(ID, gMeasuredValues(Monitor))).

gDay(Date) ->
  {{_, _, Day}, {_, _, _}} = Date,
  Day.

calcMean(Measures) ->
  case length(Measures) of
    0 ->
      nothing;
    Len ->
      lists:foldl(
        fun(X, Acc) -> Acc + gMeasureValue(X) end, 0, Measures) / Len
  end.

calcGradient(Values) ->
  case length(Values) of
    0 -> 0;
    1 -> 0;
    Len ->
      grad(Values, Len - 1, 0)
  end.

grad([H1, H2 | T], N, Acc) ->
  grad(T, N, Acc + abs(gMeasureValue(H1) - gMeasureValue(H2)) / N);
grad(_, _, Acc) ->
  Acc.

getMax([H | T]) ->
  getMax_(T, H);
getMax([]) -> 0.
getMax_([H | T], Max) ->
  case element(2, H) > element(2, Max) of
    true -> getMax_(T, H);
    false -> getMax_(T, Max)
  end;
getMax_([], Max) ->
  Max.
