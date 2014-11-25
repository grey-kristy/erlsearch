-module(es_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, erlsearch).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(GET_ENV(Key), utils:get_env(?APP, Key)).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, { {one_for_one, 5, 10}, [ranch_spec(?GET_ENV(http_port)), ?CHILD(es_server, worker)]} }.

%% Internal functions

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            static("js"),
            static("css"),
            {"/api/:action", es_front, []},
            {"/:action", es_front, []},
            {"/", es_front, []},
            {'_', notfound, []}
        ]}
    ]).

ranch_spec(Port) ->
    ranch:child_spec(http, 20, ranch_tcp, [{port, Port}], cowboy_protocol, [{env, [{dispatch, dispatch_rules()}]}]).

static(Filetype) ->
    {lists:append(["/", Filetype, "/[...]"]), cowboy_static, [
        {directory, {priv_dir, ?APP, [list_to_binary(Filetype)]}},
        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
    ]}.

