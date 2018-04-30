%%%-------------------------------------------------------------------
%%% @author ajopek
%%% @doc
%%%   This module provides functionality of:
%%%     - Collecting data from fictional air quality measurement stations
%%%     - Processing those data
%%%   This version does not use OTP intentionally.
%%% @end
%%% Created : 18. Apr 2018 22:09
%%%-------------------------------------------------------------------
-module(pollution).
-author("ajopek").

-include("../include/pollution.hrl").
%% API
-export([addStation/2,
         createMonitor/0,
         addValue/2,
         removeValue/2,
         getOneValue/2,
         getStationMean/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Create new monitor.
%%% @end
%%%-------------------------------------------------------------------

-spec createMonitor() -> monitor().
createMonitor() ->
  #monitor{}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Add station to provided monitor.
%%% Returns error if station with provided name or coords already exists.
%%% @end
%%%-------------------------------------------------------------------
-spec addStation(monitor(), station()) -> monitor() | {error, station_exists}.
addStation(#monitor{name_to_coords = Name_to_coords, coords_to_station = Coords_to_station} = Monitor,
           Station = #station{name = Name, geo_coordinates = Coords}) ->
  case findStation(Monitor, {coords, Coords}) of
    {error, _} ->
      Monitor#monitor{name_to_coords = Name_to_coords#{Name => Coords},
                      coords_to_station =  Coords_to_station#{Coords => Station}};
    {ok, _}    -> {error, station_exists}
  end.

%%%-------------------------------------------------------------------
%%% @doc
%%% Add station to provided monitor.
%%% Returns error if station with provided name or coords already exists.
%%% @end
%%%-------------------------------------------------------------------
-spec addValue(monitor(), {geo_coords(), measurement()}) -> monitor() | {error, exists}.
addValue(#monitor{coords_to_station = Coords_to_station} = Monitor,
         {Coords, #measurement{datetime = Date, type = Type} = Measurement}) ->
  case findValue(Monitor, #{date => Date, type => Type, station => {coords, Coords}})of
    nil ->
      #{Coords := Station} = Coords_to_station,
      #station{measurements = OldMeasurements} = Station,
      Monitor#monitor{coords_to_station =
        Coords_to_station#{Coords := Station#station{measurements = [Measurement | OldMeasurements]}}};
    _ -> {error, exists}
  end.

removeValue(#monitor{coords_to_station = Coords_to_station} = Monitor,
            {Coords, #measurement{datetime = Date, type = Type} = Measurement}) ->
  case findValue(Monitor, #{date => Date, type => Type, station => {coords, Coords}})of
    nil -> {error, exists};
    _ ->
      #{Coords := Station} = Coords_to_station,
      #station{measurements = OldMeasurements} = Station,
      Monitor#monitor{coords_to_station =
        Coords_to_station#{Coords := Station#station{measurements = OldMeasurements -- [Measurement]}}}
  end.

getOneValue(#monitor{} = Monitor, #{date := Date, type := Type, station := Station} = Measurement) ->
  findValue(Monitor, Measurement).

getStationMean(#monitor{} = Monitor, #{type := Type, station := Station_id}) ->
  case findStation(Monitor, Station_id) of
    {ok, #station{measurements = Measurements}} ->
      Filtered = lists:filter(fun (#measurement{type = Type}) -> true; (_) -> false end, Measurements),
      {Sum, Elem_num} =
        lists:foldl(fun (#measurement{value = Value}, {Sum_acc, Num_acc}) -> {Sum_acc + Value, Num_acc + 1} end,
                    {0, 0},
                    Filtered),
      Sum/Elem_num;
    Error -> Error
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%-------------------------------------------------------------------
%%% @doc
%%% Find station in provided monitor, by name or geographical coordinates.
%%% @end
%%%-------------------------------------------------------------------
-spec findStation(monitor(), {name, station_name()}) -> {ok, station()} | {error, badname};
                 (monitor(), {coords, geo_coords()}) -> {ok, station()} | {error, badcoords}.

findStation(#monitor{name_to_coords = Name_to_coords} = Monitor,
            {name, Name}) ->
  case maps:get(Name, Name_to_coords, nostation) of
      nostation -> {error, badname};
      Coords        -> findStation(Monitor, {coords, Coords})
  end;
findStation(#monitor{coords_to_station = Coords_to_station}, {coords, Coords}) ->
  case maps:get(Coords, Coords_to_station, nokey) of
    nokey -> {error, badcoords};
    Station       -> {ok, Station}
  end.

-spec findValue(monitor(), #{date => calendar:datetime(),
                             type => measure_type(),
                             station => {name, station_name()} | {coords, geo_coords()} }) ->
  measurement() | nil.
findValue(#monitor{} = Monitor, #{date := Date, type := Type, station := Station_identifier}) ->
  case findStation(Monitor, Station_identifier) of
    {ok, #station{measurements = Measurements}} ->
      Found_measurements =
        lists:filter(fun(#measurement{datetime = Measure_date, type = Measure_type}) ->
                        if
                          (Measure_date == Date) and (Measure_type == Type) -> true;
                          true                                            -> false
                        end
                     end, Measurements),
      case Found_measurements of
        [] -> nil;
        [Found_measurement] -> Found_measurement
      end;
    {error, badname} -> nil
  end.

