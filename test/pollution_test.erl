%%%-------------------------------------------------------------------
%%% @author ajopek
%%% @doc
%%%   Tests of pollution module.
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("ajopek").

%%-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("pollution.hrl").

%% tests description
-export([]).

%% test cases
-export([]).

%% Test data
-define(COORD1, {20.3, 15.7}).
-define(COORD2, {54.3, 32.7}).
-define(DATETIME1, {{2018, 08, 12}, {1, 2, 3}}).
-define(DATETIME2, {{2018, 06, 01}, {5, 6, 7}}).

%%%===================================================================
%%% Tests description
%%%===================================================================

functionality_test_() ->
  [].

%%%====================================================================
%%% Test cases
%%%====================================================================

create_monitor_test() ->
  ?assertEqual(#monitor{}, pollution:createMonitor()).

addStation_test() ->
  Monitor = #monitor{name_to_coords = #{"Name" => ?COORD1},
                     coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[]}}},
  ?assertEqual(Monitor ,pollution:addStation(#monitor{}, #station{name = "Name", geo_coordinates = ?COORD1})).

addStation_duplicate_stations_test() ->
  Monitor = #monitor{name_to_coords = #{"Name" => ?COORD1},
                     coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[]}}},
  ?assertEqual({error, station_exists},
               pollution:addStation(Monitor, #station{name = "Name", geo_coordinates = ?COORD1})).

addValue_test() ->
  Monitor = #monitor{name_to_coords = #{"Name" => ?COORD1},
            coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[]}}},
  Expected = #monitor{name_to_coords = #{"Name" => ?COORD1},
             coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[#measurement{datetime = ?DATETIME1}]}}},
  ?assertEqual(Expected, pollution:addValue(Monitor, {?COORD1, #measurement{datetime = ?DATETIME1}})).

%% add value not uniq

removeValue_test() ->
  Monitor = #monitor{name_to_coords = #{"Name" => ?COORD1},
            coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[#measurement{datetime = ?DATETIME1}]}}},
  Expected = #monitor{name_to_coords = #{"Name" => ?COORD1},
             coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[]}}},
  ?assertEqual(Expected, pollution:removeValue(Monitor, {?COORD1, #measurement{datetime = ?DATETIME1}})).

%% remove non existing

getOneValue_test() ->
  Monitor = #monitor{name_to_coords = #{"Name" => ?COORD1},
    coords_to_station = #{?COORD1 => {station,"Name",?COORD1,[#measurement{datetime = ?DATETIME1}]}}},
  Monitor2 = pollution:addValue(Monitor, {?COORD1, #measurement{datetime = ?DATETIME2}}),
  ?assertEqual(#measurement{datetime = ?DATETIME2, type = "PM10", value = 0.0},
               pollution:getOneValue(Monitor2, #{date => ?DATETIME2, type => "PM10", station => {coords, ?COORD1}})).

getStationMean_test() ->
