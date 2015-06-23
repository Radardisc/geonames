-module(geonames_client).


-compile(export_all).

start() -> application:start(geonames_client).

rpc(Method, Args) ->
    rpc:call(node_name(), module_name(), Method, Args).


module_name() ->
    geonames.


node_name() -> application:get_env(geonames_client,geonames_server_node,node() ).


extract_key(Key, List) ->
    rpc(extract_key, [Key,List]).


neighbours(Country) ->
    rpc(neighbours, [Country]).


cities_in_area_min_pop(CountryCode, AdminCode1, AdminCode2, MinPop) ->
    rpc(cities_in_area_min_pop,
        [CountryCode,AdminCode1,AdminCode2,MinPop]).


cities_in_area_min_pop(CountryCode, AdminCode1, MinPop) ->
    rpc(cities_in_area_min_pop, [CountryCode,AdminCode1,MinPop]).


cities_in_area(CountryCode, AdminCode1, AdminCode2) ->
    rpc(cities_in_area, [CountryCode,AdminCode1,AdminCode2]).


cities_in_area(CountryCode, AdminCode1) ->
    rpc(cities_in_area, [CountryCode,AdminCode1]).


cities_in_area(AreaPlist) ->
    rpc(cities_in_area, [AreaPlist]).


cities_in_country(CountryCode) ->
    rpc(cities_in_country, [CountryCode]).


country_areas(CountryCode, AdminLevels) ->
    rpc(country_areas, [CountryCode,AdminLevels]).


country_areas(CountryCode) ->
    rpc(country_areas, [CountryCode]).
    
country_sub_areas(CountryCode,AdminCode1) ->
    rpc(country_sub_areas, [CountryCode,AdminCode1]).


continent(Name, Key) ->
    rpc(continent, [Name,Key]).


continent(Name) ->
    rpc(continent, [Name]).


continents() ->
    rpc(continents, []).
    
countries_in_continent(Cont) ->
    rpc( countries_in_continent, [ Cont ] ).


country_details(Code) ->
    rpc(country_details, [Code]).


country_codes() ->
    rpc(country_codes, []).


cities_for_country(CountryCode) ->
    rpc(cities_for_country, [CountryCode]).


