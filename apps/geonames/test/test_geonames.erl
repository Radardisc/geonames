-module(test_geonames).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, geonames).

cfg() ->
    [ 
        { geonames, [
            { build_db, if_required },
            { mnesia_dir, "./mnesia"},
            { data_dir, "../data"},
            { import, all }
        ] }
    ].
    
setup() -> 
    otp_utils:start_apps(cfg()).
    
teardown(_) -> ok.

geonames_test_() ->
   {setup, fun setup/0, fun teardown/1,
        [ 
            {timeout, 300, fun run/0 }
        ] 
    }.
%%%.
run() ->

    CCS = ?MOD:country_codes(),
    
    ?debugFmt("~p", [ CCS ] ),
    
    CountryCode = "GB",
    
    Areas = ?MOD:country_areas(CountryCode,1),
    
    
    lists:foreach( fun( AreaName ) ->
        Cities = ?MOD:cities_in_area_min_pop(CountryCode, AreaName,250000),
        ?debugFmt("\n~p / ~p \n ~p", [ CountryCode,  AreaName, Cities] )
    end , Areas ).
    
    
show_cities(CountryCode) ->
    
    Areas  = ?MOD:country_areas(CountryCode),
    
    lists:foreach( fun( AreaPlist ) ->
        Cities = ?MOD:cities_in_area(AreaPlist),
        Names = ?MOD:extract_key(name, Cities),
        %?debugFmt("~s ~p \n ~p", [ CountryCode, proplists:get_value(admin1,AreaPlist ), Names ] )
        ok
    end, Areas ).
    
    
show_neighbours() ->
    CCS = ?MOD:country_codes(),
    
    
    %{ok, IO} = file:open("/tmp/dump",[write ]),
    
    lists:foreach( fun( Code ) ->
        ?debugFmt( "~p ~p", [ Code, ?MOD:neighbours(Code) ] )
    end, CCS ).
    
    
show_continents() ->
    Continents = ?MOD:continents(),
    
    lists:foreach( fun( Continent ) ->
        Countries = ?MOD:continent(Continent,country_name),
        ?debugFmt("~p : ~p", [ Continent, Countries ] )
    end, Continents ).
    
    
    

