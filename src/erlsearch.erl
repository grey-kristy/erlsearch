-module(erlsearch).
-behaviour(application).

%% application callbacks
-export([start/2, stop/1, start/0]).

start() ->
    application:ensure_all_started(?MODULE).

start(normal, _StartArgs) ->
    es_sup:start_link().

stop(_State) ->
    ok.
