-module(scan).

-export([load/0, get_docs/0, make_index/1]).

-define(DOC_TABLE, doc_table).
-define(DOC_FILE, "priv/doc_table.ets").


load() ->
    ets:file2tab(?DOC_FILE).

get_docs() ->
    case ets:info(?DOC_TABLE, size) of
        undefined -> ets:new(?DOC_TABLE, [set, named_table]);
        _         -> ets:delete_all_objects(?DOC_TABLE)
    end,
    scan_docs(?DOC_TABLE),
    ets:tab2file(?DOC_TABLE, ?DOC_FILE).

make_index(Table) ->
    make_index(Table, ets:first(Table)).

make_index(Table, '$end_of_table') ->    
    ok;

make_index(Table, Key) ->
    io:format("~p = ~p~n", [binary:part(Key, {0, 3}), Key]),
    make_index(Table, ets:next(Table, Key)).

scan_docs(Table) ->
    lager:info("start get"),
    inets:start(),
    {ok, {{_Proto, 200, "OK"}, _Headers, Body}} = httpc:request("http://www.erlang.org/doc/man/array.html"),
    {_String, _Attributes, All} = mochiweb_html:parse(Body),
    {_, Bd} = get_value(<<"body">>, All),
    {_, R2} = get_value_by_attr(<<"div">>, <<"id">>, <<"container">>, Bd),
    {_, R3} = get_value_by_attr(<<"div">>, <<"id">>, <<"leftnav">>, R2),
    {_, R4} = get_value_by_attr(<<"div">>, <<"class">>, <<"innertube">>, R3),
    {_, R5} = get_value_by_attr(<<"ul">>, <<"class">>, <<"flipMenu">>, R4),
    [ets:insert(Table, get_section(S)) || S <- get_top(R5)].


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
    