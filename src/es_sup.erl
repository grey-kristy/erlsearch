-module(es_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, erlsearch).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(GET_ENV(Key), es_utils:get_env(?APP, Key)).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, { {one_for_one, 5, 10}, [ranch_spec(?GET_ENV(http_port)), ?CHILD(es_server, worker)]} }.

%% Internal functions

dispatch_rules() ->
    set_priv_path(?APP),
    cowboy_router:compile([
        {'_', [
            static("js"),
            static("css"),
            {"/favicon.ico", cowboy_static, {priv_file, ?APP, "favicon.ico"}},
            {"/api/:action", es_front, []},
            {"/:action", es_front, []},
            {"/", es_front, []},
            {'_', es_notfound, []}
        ]}
    ]).

ranch_spec(Port) ->
    ranch:child_spec(http, 20, ranch_tcp, [{port, Port}], cowboy_protocol, [{env, [{dispatch, dispatch_rules()}]}]).

set_priv_path(AppName) ->
    AppDir = filename:dirname(code:which(AppName)),
    code:add_path(AppDir).

static(Filetype) ->
    {"/" ++ Filetype ++ "/[...]", cowboy_static, {priv_dir, ?APP, Filetype}}.


