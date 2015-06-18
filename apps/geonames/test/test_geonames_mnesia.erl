
-module(test_geonames_mnesia).
-author('Rascerl <rascerl>').

-include_lib("eunit/include/eunit.hrl").

-define(MOD, geonames_mnesia).

cfg() ->
    [ 
        { common, [ 
            {mnesia,
                [ 
                    {options, [ start ] } ,
                    {modules, [
                        { ?MOD, init }
                    ]} 
                ]
            }
        ] }
    ].
    
setup() -> 
    otp_utils:start_apps(cfg()).
    
teardown(_) -> ok.
%%%.
%%%' TEST GENERATOR
%% @spec geoname_mnesia_test_() -> List
%% where
%%       List = [term()]
geoname_mnesia_test_() ->
   {foreach, fun setup/0, fun teardown/1,
        [ 
            fun run/0
        ] 
    }.
%%%.
run() ->
    
    ok.
    

