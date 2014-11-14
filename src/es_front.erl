-module(es_front).

-behaviour(cowboy_http_handler).

-export([cook_body/1]).

%% Cowboy http_handler callbacks
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State) ->
    {Req2, Post, _Get} = utils:log_request(Req),
    {Action, Req3} = cowboy_req:binding(action, Req2),
    [SearchReq] = utils:get_post(Post, [request]),
    Out = case size(SearchReq) > 0 of
        true  -> search(SearchReq);
        false -> cook_form()
    end,
    ok(Req3, State, Out).

%% Internal functions

search(SearchReq) ->
    Out = fun(Name, Ref) ->
        html:li(html:a(<<"http://www.erlang.org/doc/man/", Ref/binary>>, Name, {target, blanc}))
    end,
    R = [Out(Name, Ref) || {Name, Ref} <- es_server:search(SearchReq)],
    [cook_form(), html:hdiv(html:ul(R), {class, row})].

cook_form() ->
    Input = html:input([
        {type, text}, 
        {name, request}, 
        {id, request_id}, 
        {placeholder, "search"}, 
        {class, "form-control"}
    ]),
    Form = [
        html:hdiv(Input, {class, "form-group"}),
        html:button("Search", [{class, "btn btn-default"}, {type, submit}])
    ],
    html:hdiv( html:form(Form, [
        {method, post},
        {class, "navbar-form navbar-left"}, 
        {role, search}
    ]), {class, row}).
    

cook_body(Body) ->
    Title = <<"Erlang Fast Search">>,
    Head = html:head([
        html:title(Title),
        html:hlink([{rel, stylesheet}, {type, 'text/css'}, {href, '/css/es.css'}]),
        html:hlink([{rel, stylesheet}, {type, 'text/css'}, {href, '/css/bootstrap.3.3.1.min.css'}])
    ]),
    Content = [Body, <<"<hr>">>, html:footer(html:p("&copy; GreyKristy 2014"))],
    HTML = html:html([
        Head, html:body( html:hdiv(Content, {class, container})   )
    ]),
    Type = <<"<!DOCTYPE html>\n">>,
    [Type, HTML].

ok(Req, State, Body) ->
    Type = <<"<!DOCTYPE html>\n">>,
    Headers = [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, [Type, cook_body(Body)], Req),
    {ok, Req2, State}.
