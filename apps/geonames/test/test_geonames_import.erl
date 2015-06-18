
-module(test_geonames_import).
-author('Rascerl <rascerl>').

-include_lib("eunit/include/eunit.hrl").

-define(MOD, geonames_import).

setup() -> ok.
teardown(_) -> ok.
%%%.
%%%' TEST GENERATOR
%% @spec geonames_import_test_() -> List
%% where
%%       List = [term()]
geonames_import_test_() ->
   {foreach, fun setup/0, fun teardown/1,
        [ 
            fun run/0
        ] 
    }.
%%%.
run() ->
    ok.
    

