%%%-------------------------------------------------------------------
%%% @author ajopek
%%% @doc
%%%   Types and records declaration for pollution module.
%%% @end
%%% Created : 18. Apr 2018 23:21
%%%-------------------------------------------------------------------
-author("ajopek").

%%-ifndef(POLLUTION_HRL).
%%-define(POLLUTION_HRL, 1).

-type station_name() :: string().
-type geo_coords()   :: {float(), float()}.
-type measure_type() :: string().


-record(measurement, {type         = "PM10"             :: measure_type(),
                      value        = 0.0                :: float(),
                      datetime     = erlang:localtime() :: calendar:datetime()}).
-type measurement() :: #measurement{}.

-record(station, {name            = ""         :: station_name(),
                  geo_coordinates = {0.0, 0.0} :: geo_coords(),
                  measurements    = []         :: [measurement()]}).
-type station() :: #station{}.

-record(monitor, {name_to_coords    = #{} :: #{station_name() => geo_coords()},
                  coords_to_station = #{} :: #{geo_coords() => station()}}).
-type monitor() :: #monitor{}.

