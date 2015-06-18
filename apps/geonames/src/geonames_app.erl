
-behaviour(application).

-include("geonames_log.hrl").

-export([start/2, stop/1]).
%%%.
%%%'   CALLBACKS
start(_StartType, _StartArgs) ->
   case geonames_sup:start_link()of
        {ok, _Pid} = Res ->
            post_sup_init(),
            Res;
        Err ->
            Err
    end.
        

stop(_State) ->
  ok.
  
  
ge(K,Def) -> application:get_env(geonames,K,Def).


post_sup_init() ->
    build_db(ge(build_db,undefined) ),
    geonames_mnesia:wait_tables().
    
    
build_db(undefined ) -> ok;
build_db(if_required) ->
    load_mnesia(),
    case geonames_mnesia:tables_exist() of
        false ->
            geonames_mnesia:init( [ { disc_copies, [ node() ] } ] ),
            geonames_mnesia:wait_tables(),
            geonames_import:build_db();
        true ->
            ok
    end;
build_db(true) ->
    load_mnesia(),
    geonames_mnesia:delete_tables(),
    geonames_mnesia:init( [ { disc_copies, [ node() ] } ] ),
    geonames_mnesia:wait_tables(),
    geonames_import:build_db();
build_db(Other) ->
    ?WARN("Invalid db command: ~p", [ Other ] ),
    ok.

load_mnesia() ->
    MnesiaDir = ge(mnesia_dir ,"./data" ),
    
    application:load(mnesia),
    application:set_env(mnesia,dir, MnesiaDir ),
    mnesia:create_schema([node()]),
    Self = self(),
    spawn( fun() ->
        application:start(mnesia),
        Self ! mnesia_started
    end ),
    
    receive 
        mnesia_started ->
            ok
    after
        10000 ->
            ?MSG("Starting mnesia timed out. Be careful!", [] )
    end.
    