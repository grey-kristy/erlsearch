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
    {Req2, Post, Get} = es_utils:log_request(Req),
    {Action, ReqN} = cowboy_req:binding(action, Req2),
    case Action of
        <<"suggest">> -> js_out(ReqN, State, suggest(Get));
        _             -> ok(ReqN, State, search(Post, Get))
    end.

%% Internal functions

suggest(Get) ->
    [Query] = es_utils:get_get(Get, ['query']),
    R = [{[{value, Name}, {data, Ref}]}  || {Name, Ref} <- es_server:search(Query)],
    [{'query', Query}, {suggestions, R}].

search(Post, Get)  ->
    [SearchReq] = es_utils:get_request(Post, Get, [request]),
    case size(SearchReq) > 0 of
        true  -> do_search(SearchReq);
        false -> cook_form()
    end.

do_search(SearchReq) ->
    Out = fun(Name, Ref) ->
        html:li(html:a(<<"http://www.erlang.org/doc/man/", Ref/binary>>, Name, {target, blanc}))
    end,
    R = [Out(Name, Ref) || {Name, Ref} <- es_server:search(SearchReq)],
    [cook_form(), html:hdiv(html:ul(R), {class, row})].

cook_form() ->
    Form = [
        html:input([{type, text}, {name, request}, {id, "autocomplete-ajax"}, {placeholder, "search erlang function"}, {size, 30}]),
        html:input([{type, text}, {disabled, disabled}, {id, "autocomplete-ajax-x"}, {size, 30}])
    ],
    [
        html:hdiv(html:hdiv(html:form(Form, {method, post}), {class, search}), {class, container}),
        html:hdiv("", [{id, result}, {class, container}])
    ].

cook_body(Body) ->
    Title = <<"Erlang Fast Search">>,
    Head = html:head([
        html:title(Title),
        html:hlink([{rel, stylesheet}, {type, 'text/css'}, {href, "/css/es.css"}]),
        html:script("", [{type, "text/javascript"}, {src, "/js/jquery-1.8.2.min.js"}]),
        html:script("", [{type, "text/javascript"}, {src, "/js/jquery.autocomplete.js"}]),
        ga()
    ]),
    Footer = html:footer(html:p(["&copy; ", html:a("https://github.com/grey-kristy", "GreyKristy"), " 2014"])),
    Content = [Body, Footer],
    HTML = html:html([
        Head, html:body( [
%%            html:hdiv(Content, {class, container}),
            Content,
            html:script("", [{type, "text/javascript"}, {src, "/js/suggest.js"}])
        ])
    ]),
    HTML.

ga() ->
    Code = "
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-57094362-1', 'auto');
  ga('send', 'pageview');

",
    html:script(Code).
    
ok(Req, State, Body) ->
    Type = <<"<!DOCTYPE html>\n">>,
    Headers = [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, [Type, cook_body(Body)], Req),
    {ok, Req2, State}.

js_out(Req, State, JSON) ->
    Body = jiffy:encode({JSON}),
    {ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Body, Req),
    {ok, Req2, State}.

