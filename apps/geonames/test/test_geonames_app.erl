

-module(test_geonames_app).
-author('Rascerl <rascerl>').

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").
-define(MODNAME, geonames).


config() -> [].

setup() ->
    setup(config()).

setup(Cfg) ->
    lists:foreach( fun load_cfg_element/1, Cfg ),
    Cfg.

teardown(Cfg) ->
    lists:foreach( fun stop_cfg_element/1, Cfg).
    
load_cfg_element(App) when is_atom(App) -> 
    case application:start(App) of
        ok -> ok;
        Other ->
            ?debugFmt("Starting of application ~p failed", [App ] ),
            ?assertMatch(ok, Other)
    end;
load_cfg_element( {App, Config} ) ->
    case application:load(App) of
        ok -> 
            lists:foreach( fun({K,V}) ->
                application:set_env(App, K, V)
            end, Config ),
            load_cfg_element(App);
        Other ->
            ?debugFmt("Loading of application ~p failed", [App ] ),
            ?assertMatch(ok, Other)
    end.
    
stop_cfg_element( {App, _} ) -> 
    stop_cfg_element( App ); 
stop_cfg_element( App) -> 
    ?assertMatch(ok, application:stop(App) ),
    ?assertMatch(ok, application:unload(App) ).
    
    

geonames_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [ 
            fun run/0
        ] 
    }.


run() ->
    ok.
    
    
