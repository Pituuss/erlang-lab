defmodule PollutionData do
  def loadStationsToServer do
    :supervisor_OTP.start_link()
    PollutionData.identifyStations()
    |> Map.to_list()
    |> Enum.each(
    fn x ->
      :pollution_server_OTP.addStation(elem(x,1) |> to_charlist , elem(x,0))
    end)
    end
def loadMeasurementsToServer do
    PollutionData.importLinesFromCSV
    |> Enum.each(
    fn(x) ->
      :pollution_server_OTP.addValue(x.location,x.datetime,'PM10',x.pollutionLevel)
    end)
  end

  def startAndLoadServer() do
    IO.puts " loading stations: #{calcTime fn -> PollutionData.loadStationsToServer end}"
    IO.puts " loading measurements: #{calcTime fn -> PollutionData.loadMeasurementsToServer end}"
  end

  def calcTime(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def parseLine(line) do
    [date_string, hour_string, cord1_string, cord2_string, val_string] = String.split(line, [","])
    date = {PollutionData.parseDate(date_string), PollutionData.parseHour(hour_string)}
    cord = {String.to_float(cord1_string), String.to_float(cord2_string)}
    val = String.to_integer(val_string) / 1

    %{:datetime => date, :location => cord, :pollutionLevel => val}
  end

  def parseStation(line) do
    [_, _, cord1, cord2, _] = String.split(line, [","])
    {String.to_float(cord1), String.to_float(cord2)}
  end

  def parseDate(date_string) do
    String.split(date_string, "-")
    |> Enum.reverse()
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def parseHour(hour_string) do
    String.split(hour_string, ":")
    |> Enum.map(&String.to_integer/1)
    |> (&(&1 ++ [0])).()
    |> List.to_tuple()
  end

  def importLinesFromCSV do
    File.read!("./pollution.csv")
    |> String.split("\r\n")
    |> Enum.map(&PollutionData.parseLine/1)
  end

  def identifyStations do
    File.read!("./pollution.csv")
    |> String.split("\r\n")
    |> Enum.map(&PollutionData.parseStation/1)
    |> Enum.reduce(%{}, fn x, acc ->
      Map.put(acc, x, "station_#{elem(x, 0)}_#{elem(x, 1)}")
    end)
  end
end
