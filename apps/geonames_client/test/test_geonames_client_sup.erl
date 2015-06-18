

-module(test_geonames_client_sup).


-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").
-define(MODNAME, geonames_client).


setup() ->
    ok.
    

teardown(_Cfg) ->
    ok.
    


geonames_client_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [ 
            fun run/0
        ] 
    }.


run() ->
    ok.
    
    
