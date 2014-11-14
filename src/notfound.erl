-module(notfound).
-behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
    init/3,
    handle/2,
    terminate/3
]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Body = html:h2(<<"404 Page Not Found">>),
    {ok, Req2} = cowboy_req:reply(404, [], es_front:cook_body(Body), Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

