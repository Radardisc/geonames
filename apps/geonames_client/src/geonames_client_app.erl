

-module(geonames_client_app).
-behaviour(application).

-export([start/0,start/2, stop/1]).
%%%.
%%%'   CALLBACKS
start(_StartType, _StartArgs) ->
  geonames_client_sup:start_link().

stop(_State) ->
  ok.


start() -> application:start(geonames_client).