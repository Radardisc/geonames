

-module(test_geonames_sup).
-author('Rascerl <rascerl>').

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").
-define(MODNAME, geonames).


setup() ->
    ok.
    

teardown(_Cfg) ->
    ok.
    


geonames_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [ 
            fun run/0
        ] 
    }.


run() ->
    ok.
    
    
