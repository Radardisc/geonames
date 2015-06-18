

-module(test_geonames_parser).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, geonames_parser).

setup() -> application:start(geonames).
teardown(_) -> application:stop(geonames).
%%%.
%%%' TEST GENERATOR
%% @spec geonames_parser_test_() -> List
%% where
%%       List = [term()]
geonames_parser_test_() ->
   {foreach, fun setup/0, fun teardown/1,
        [ 
            %fun chomp/0
            %,
            %fun admin1/0
           %,fun admin2/0
           %fun countries/0
           %fun cities/0
        ] 
    }.
    
    
f(F) ->  filename:join(["../data/",F]).

chomp() ->
    ?assertMatch(<<"2544331">>,?MOD:chomp(<<"2544331">>) ).
    
admin1() -> print_file("admin1CodesASCII.txt", parse_admin1).
admin2() -> print_file("admin2Codes.txt", parse_admin2).
    
%%%.
timezones() ->
    TimeZones = f( "timeZones.txt" ),
     ?MOD:parse_time_zones( TimeZones, fun( Rec ) ->
        ?debugFmt("~p", [ Rec ] )
    end ).
    
countries() ->
    print_file("countryInfo.txt", parse_countries).
    
print_file(File,Parser) ->
    FileToPrint = f( File ),
    ?MOD:Parser( FileToPrint , fun( Rec ) ->
        ?debugFmt("~p", [ Rec ] )
    end ).
    
    
    
    
cities() ->
    print_file("cities1000.txt", parse_geonames).
    

