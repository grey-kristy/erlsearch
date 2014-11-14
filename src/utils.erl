-module(utils).

-export([to_int/1, to_str/1, to_bin/1]).
-export([get_post/2, get_get/2, get_request/3]).
-export([log_request/1]).
-export([get_ip/1, ip_to_str/1]).
-export([get_env/2, get_env/3]).


-define(debug(Channel, Msg), lager:debug([{channel, Channel}], Msg)).
-define(debug(Channel, Msg, Args), lager:debug([{channel, Channel}], Msg, Args)).


to_int(Int) when is_integer(Int) -> Int;
to_int(Int) when is_binary(Int) -> to_int(binary_to_list(Int));
to_int(null) -> 0;
to_int(Int) when is_list(Int) ->
    try list_to_integer(Int)
    catch
        error:badarg -> 0
    end.

to_str(Int) when is_integer(Int) -> integer_to_list(Int);
to_str(Int) when is_number(Int) -> lists:flatten(io_lib:format("~p", [Int]));
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(Str) when is_list(Str) -> lists:flatten(Str).

to_bin(Any) -> list_to_binary(to_str(Any)).


get_ip(Req) ->
    Hdr = cowboy_req:headers(Req),
    H2 = element(2, Hdr),
    {IP,_} = element(8, H2),
    IP.

ip_to_str(IP) ->
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".").


%%
%% get optional post parameters
%%

get_post(Post, ArgList) ->       
    [proplists:get_value(list_to_binary(atom_to_list(Key)), Post, <<"">>) || Key <- ArgList].

get_get(Req, ArgList) ->       
    {Get, _} = cowboy_req:qs_vals(Req),
    [proplists:get_value(list_to_binary(atom_to_list(Key)), Get, <<"">>) || Key <- ArgList].

get_request(Post, Get, ArgList) ->       
    [proplists:get_value(list_to_binary(atom_to_list(Key)), Post ++ Get, <<"">>) || Key <- ArgList].

log_request(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Post = cowboy_http:x_www_form_urlencoded(Body),
    {QS,   Req3} = cowboy_req:qs(Req2),
    {Url,  Req4} = cowboy_req:host_url(Req3),
    {Path, Req5} = cowboy_req:path(Req4),
    case length(utils:to_str(Post)) of
        0 ->
            case length(utils:to_str(QS)) of
                0 -> ?debug(access, "~ts~ts", [to_str(Url), to_str(Path)]);
                _ -> ?debug(access, "~ts~ts GET: ~ts", [to_str(Url), to_str(Path), to_str(QS)])
            end;
        _ ->
            case length(utils:to_str(QS)) of
                0 -> ?debug(access, "~ts~ts POST: ~ts", [to_str(Url), to_str(Path), Body]);
                _ -> ?debug(access, "~ts~ts GET: ~ts POST: ~ts", [to_str(Url), to_str(Path), to_str(QS), Body])
            end
    end,
    {Req5, Post, cowboy_http:x_www_form_urlencoded(QS)}.

%%
%% Application utils
%%
get_env(App, Key) ->
    {ok, Value} = application:get_env(App, Key),
    Value.

get_env(App, Key, Default) -> application:get_env(App, Key, Default).

