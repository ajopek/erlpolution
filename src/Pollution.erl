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
-module('Pollution').
-author("ajopek").

-include("../include/pollution.hrl").
%% API
-export([]).


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
  {ok, #monitor{}}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Add station to provided monitor.
%%% Returns error if station with provided name or coords already exists.
%%% @end
%%%-------------------------------------------------------------------
-spec addStation(monitor(), station()) -> monitor().
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
addValue(#monitor{coords_to_station = Coords_to_station, name_to_coords } = Monitor,
         {Coords, #measurement{datetime = Date, type = Type} = Measurement}) ->
  case findValue(Monitor, #{date => Date, type => Type, station => {coords, Coords}})of
    nil ->
      #{Coords := Station} = Coords_to_station,
      #station{measurements = OldMeasurements} = Station,
      Monitor#monitor{coords_to_station =
        Coords_to_station#{Coords := Station#station{measurements = [OldMeasurements | Measurement]}}};
    _ -> {error, exists}
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
  case maps:get(Name_to_coords, Name) of
      {badkey, Key} -> {error, badname};
      Coords        -> findStation(Monitor, {coords, Coords})
  end;
findStation(#monitor{coords_to_station = Coords_to_station}, {coords, Coords}) ->
  case maps:get(Coords_to_station, Coords) of
    {badkey, Key} -> {error, badcoords};
    Station       -> {ok, Station}
  end.

-spec findValue(monitor(), #{date => datetime(),
                             type => measure_type(),
                             station => {name, station_name()} | {coords, geo_coords()} }) ->
  measurement() | nil.
findValue(#monitor{} = Monitor, #{date => Date, type => Type, station => Station_dentifier}) ->
  case findStation(Monitor, Station_dentifier) of
    {ok, #station{measurements = Measurements}} ->
      [Found_measurement] =
        lists:filter(fun(#measurement{datetime = Measure_date, type = Measure_type} = Measurement) ->
                        if
                          Measure_date =:= Date and Measure_type =:= Type -> true;
                          true                                            -> false
                        end
                     end, Measurements),
      Found_measurement;
    {error, badname} -> nil
  end.

