-module(geonames).

-include("geonames_log.hrl").

-export([cities_for_country/1,country_codes/0,country_details/1]).
-export([continents/0, continent/1,continent/2,neighbours/1,country_areas/1,country_areas/2]).

-export([cities_in_country/1,cities_in_area/1,cities_in_area/2,cities_in_area/3,extract_key/2]).
-export([cities_in_area_min_pop/3,cities_in_area_min_pop/4,country_sub_areas/2,countries_in_continent/1]).

-define(DB,geonames_mnesia).


cities_for_country(CountryCode) -> ?DB:read(country_cities, CountryCode).
    
country_codes() ->?DB:read(country_codes, no_arg).
    
country_details(Code) ->?DB:read(country, Code).
 
continents() -> ?DB:read(continent,all).

continent(Name) -> ?DB:read(continent,Name).
continent(Name,Key) -> 
    extract_key(Key,?DB:read(continent,Name) ).
    
country_areas(CountryCode) ->
    ?DB:read(country_areas,CountryCode).
    
country_areas(CountryCode, AdminLevels ) when is_integer(AdminLevels) ->
    ?DB:read(country_areas, {CountryCode,AdminLevels }).
    
cities_in_country(CountryCode) -> ?DB:read(cities, { country,CountryCode} ).

cities_in_area(AreaPlist) -> ?DB:read(cities, {by_area,AreaPlist} ).

cities_in_area(CountryCode,AdminCode1) -> ?DB:read(cities, {by_area, CountryCode, AdminCode1} ).

cities_in_area(CountryCode,AdminCode1,AdminCode2) -> 
    ?DB:read(cities, { by_area,CountryCode, AdminCode1, AdminCode2} ).
    
countries_in_continent(Continent) ->
    ?DB:read( country, {continent, Continent } ).
    

cities_in_area_min_pop(CountryCode,AdminCode1, MinPop) ->
    ?DB:read(cities, [
        { by_area,CountryCode, AdminCode1},
        { min_population,MinPop }
    ]).
    
cities_in_area_min_pop(CountryCode,AdminCode1,AdminCode2, MinPop) ->
    ?DB:read(cities, [
        { by_area,CountryCode, AdminCode1, AdminCode2},
        { min_population,MinPop }
    ]).
    
country_sub_areas(CountryCode,AdminCode1) ->
    ?DB:read( country_areas, { sub_areas, CountryCode,AdminCode1 } ).

neighbours(Country) -> extract_key(neighbour,?DB:read( neighbours,Country) ).

extract_key(Key, List) -> [ proplists:get_value(Key,SubList,[]) || SubList <- List  ].




