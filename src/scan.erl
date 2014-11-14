-module(scan).

-export([load/0, get_docs/0, make_index/2, search/1]).

-define(DOC_TABLE, doc_table).
-define(DOC_FILE, "priv/doc_table.ets").

-define(INDEX_TABLE, index_table).
-define(INDEX_FILE, "priv/index_table.ets").


search(Word) ->
    Res = ets:lookup(?INDEX_TABLE, Word),
    [lists:nth(1, ets:lookup(?DOC_TABLE, Key)) || {_, Key} <- Res].

load() ->
    ets:file2tab(?DOC_FILE),
    ets:file2tab(?INDEX_FILE).

get_docs() ->
    create_ets(?DOC_TABLE, [ordered_set, named_table]),
    ok = scan_docs(?DOC_TABLE),
    ok = make_index(?DOC_TABLE, ?INDEX_TABLE),
    ets:tab2file(?DOC_TABLE, ?DOC_FILE),
    ets:tab2file(?INDEX_TABLE, ?INDEX_FILE).

make_index(DocTable, IndexTable) ->
    create_ets(IndexTable, [bag, named_table]),
    [make_index(IndexTable, DocTable, N, ets:first(DocTable)) || N <- lists:seq(1,10)],
    lager:info("~p indexes created OK", [ets:info(IndexTable, size)]),
    ok.

make_index(IndexTable, _DocTable, Size, '$end_of_table') ->    
    lager:info("~p (~p) indexes created OK", [Size, ets:info(IndexTable, size)]),
    ok;
make_index(IndexTable, DocTable, Size, Key) ->
    {_Module, Fun, _Arity} = split_mfa(Key),
    SubKey = get_part(Fun, Size),
%    io:format("~p ~p ~p = ~p~n", [_Module, Fun, Key, SubKey]),
    ets:insert(IndexTable, {SubKey, Key}),
    make_index(IndexTable, DocTable, Size, ets:next(DocTable, Key)).

split_mfa(MFA) ->
    [Module, MFun] = binary:split(MFA, <<":">>),
    FunAr = case binary:split(MFun, <<":">>) of
        [F]    -> F;
        [_, F] -> F
    end,
    [Fun, Arity] = binary:split(FunAr, <<"/">>),
    {Module, Fun, Arity}.

get_part(Bin, Size) ->
    case Size >= size(Bin) of
        true  -> Bin;
        false -> binary:part(Bin, {0, Size})
    end.

scan_docs(Table) ->
    inets:start(),
    lager:info("start grabbing erlang docs"),
    {ok, {{_Proto, 200, "OK"}, _Headers, Body}} = httpc:request("http://www.erlang.org/doc/man/array.html"),
    {_String, _Attributes, All} = mochiweb_html:parse(Body),
    {_, R1} = get_node(<<"body">>, All),
    {_, R2} = get_node(<<"div">>, <<"id">>, <<"container">>, R1),
    {_, R3} = get_node(<<"div">>, <<"id">>, <<"leftnav">>, R2),
    {_, R4} = get_node(<<"div">>, <<"class">>, <<"innertube">>, R3),
    {_, R5} = get_node(<<"ul">>,  <<"class">>, <<"flipMenu">>, R4),
    [ets:insert(Table, get_section(S)) || S <- get_top(R5)],
    lager:info("~p functions loaded OK", [ets:info(Table, size)]),
    ok.


get_node(_Tag, []) -> undefined;
get_node(Tag, [{Tag, Attr, Inner} | _Rest]) -> {Attr, Inner};
get_node(Tag, [_Curr | Rest]) -> get_node(Tag, Rest).
    
get_node(_Tag, _AttrName, _AttrValue, []) ->
    undefined;
get_node(Tag, AttrName, AttrValue, [{Tag, Attr, Inner} | Rest]) ->
    case proplists:get_value(AttrName, Attr) of
        AttrValue -> {Attr, Inner};
        _ -> get_node(Tag, Rest)
    end;
get_node(Tag, _AttrName, _AttrValue, [_Curr | Rest]) ->
    get_node(Tag, Rest).


get_top(Tree) -> get_top([], Tree).

get_top(Acc, []) ->
    lager:info("~p section loaded OK", [length(Acc)]),
    lists:reverse(Acc);
get_top(Acc, [{<<"li">>, Attr, Inner} | Rest]) ->
    case proplists:is_defined(<<"expanded">>, Attr) of
        true  -> get_top([{proplists:get_value(<<"title">>, Attr), Inner} | Acc], Rest);
        false -> get_top(Acc, Rest)
    end;
get_top(Acc, [_Curr | Rest]) -> 
    get_top(Acc, Rest).


get_section({SectionName, [_, {<<"ul">>, _, Tree}]}) -> 
    get_section(binary:replace(SectionName, <<" ">>, <<>>), [], Tree).

parse_ref(SectionName, [{<<"a">>, [{<<"href">>, Ref}], [Name]}]) -> 
    {<<SectionName/binary, ":", Name/binary>>, Ref}.

get_section(_Name, Acc, []) -> 
    lists:reverse(Acc);
get_section(Name, Acc, [{<<"li">>, Attr, Inner} | Rest]) ->
    case proplists:is_defined(<<"title">>, Attr) of
        true  -> get_section(Name, [parse_ref(Name, Inner) | Acc], Rest);
        false -> get_section(Name, Acc, Rest)
    end;
get_section(Name, Acc, [_Curr | Rest]) ->
    get_section(Name, Acc, Rest).


create_ets(Table, Options) ->
    case ets:info(Table, size) of
        undefined -> ets:new(Table, Options);
        _         -> ets:delete(Table), ets:new(Table, Options)
    end.
    