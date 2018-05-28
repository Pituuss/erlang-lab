defmodule PollutionDataStream do
  def loadStationsToServer do
    :supervisor_OTP.start_link()

    PollutionDataStream.identifyStations()
    |> Enum.reduce(%{}, fn x, acc ->
      Map.put(acc, x, "station_#{elem(x, 0)}_#{elem(x, 1)}")
    end)
    |> Enum.each(fn x ->
      :pollution_server_OTP.addStation(elem(x, 1) |> to_charlist, elem(x, 0))
    end)
  end

  def loadMeasurementsToServer do
    PollutionDataStream.importLinesFromCSV()
    |> Stream.each(fn x ->
      :pollution_server_OTP.addValue(x.location, x.datetime, 'PM10', x.pollutionLevel)
    end)
  end

  def startAndLoadServer() do
    IO.puts " loading stations: #{calcTime fn -> PollutionDataStream.loadStationsToServer end}"
    IO.puts " loading measurements: #{calcTime fn -> PollutionDataStream.loadMeasurementsToServer end}"
  end

  def calcTime(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def parseLine(line) do
    [date_string, hour_string, cord1_string, cord2_string, val_string] = String.split(line, [","])

    date =
      {PollutionDataStream.parseDate(date_string), PollutionDataStream.parseHour(hour_string)}

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
    File.stream!("./pollution.csv")
    |> Stream.map(&String.trim/1)
    |> Stream.map(&PollutionDataStream.parseLine/1)
  end

  def identifyStations do
    File.stream!("./pollution.csv")
    |> Stream.map(&String.trim/1)
    |> Stream.map(&PollutionDataStream.parseStation/1)
    |> Stream.uniq()
  end
end
