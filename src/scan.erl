-module(scan).

-export([get_erl/0]).


get_erl() ->
    lager:info("start get"),
    inets:start(),
    ets:new(e1, [set, named_table]),
    {ok, {Status, Headers, Body}} = httpc:request("http://www.erlang.org/doc/man/array.html"),
    {String, Attributes, All} = mochiweb_html:parse(Body),
    {_, Bd} = get_value(<<"body">>, All),
    {_, R2} = get_value_by_attr(<<"div">>, <<"id">>, <<"container">>, Bd),
    {_, R3} = get_value_by_attr(<<"div">>, <<"id">>, <<"leftnav">>, R2),
    {_, R4} = get_value_by_attr(<<"div">>, <<"class">>, <<"innertube">>, R3),
    {_, R5} = get_value_by_attr(<<"ul">>, <<"class">>, <<"flipMenu">>, R4),
    [ets:insert(e1, get_section(S)) || S <- get_top(R5)],
    ets:tab2file(e1, "e1.tab").


get_value(Key, []) ->
    undefined;
get_value(Key, [Curr | Rest]) ->
    case Curr of
        {Key, Attr, Inner} -> {Attr, Inner};
        _ -> get_value(Key, Rest)
    end.
    

get_value_by_attr(Key, AttrName, AttrValue, []) ->
    undefined;
get_value_by_attr(Key, AttrName, AttrValue, [Curr | Rest]) ->
    case Curr of
        {Key, Attr, Inner} -> 
            case proplists:get_value(AttrName, Attr) of
                AttrValue -> {Attr, Inner};
                _ -> get_value(Key, Rest)
            end;
        _ -> get_value(Key, Rest)
    end.


get_top(Tree) -> get_top([], Tree).

get_top(Acc, []) 
    -> lists:reverse(Acc);
get_top(Acc, [{<<"li">>, Attr, Inner} | Rest]) ->
    case proplists:is_defined(<<"expanded">>, Attr) of
        true  -> get_top([{proplists:get_value(<<"title">>, Attr), Inner} | Acc], Rest);
        false -> get_top(Acc, Rest)
    end;
get_top(Acc, [Curr | Rest]) -> 
    get_top(Acc, Rest).


get_section({Name, [_, {<<"ul">>, _, Tree}]}) -> get_section([], Tree).

parse_ref([{<<"a">>, [{<<"href">>, Ref}], [Name]}]) -> {Name, Ref}.

get_section(Acc, [])
    -> lists:reverse(Acc);
get_section(Acc, [{<<"li">>, Attr, Inner} | Rest]) ->
    case proplists:is_defined(<<"title">>, Attr) of
        true  -> get_section([parse_ref(Inner) | Acc], Rest);
        false -> get_section(Acc, Rest)
    end;
get_section(Acc, [Curr | Rest]) ->
    get_section(Acc, Rest).
    