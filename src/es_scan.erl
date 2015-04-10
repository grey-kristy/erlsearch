-module(es_scan).

-export([load/1, reload/2, get_docs/1, scan_docs/2, make_index/2, search/2]).

-define(DOC_FILE, "priv/doc_table.ets").
-define(INDEX_FILE, "priv/index_table.ets").


search({DocTbl, IndexTbl}, Word) ->
    Res = ets:lookup(IndexTbl, Word),
    [lists:nth(1, ets:lookup(DocTbl, Key)) || {_, Key} <- Res].

load(Heir) ->
    case ets:file2tab(?DOC_FILE) of
        {ok, DocTbl} ->
            {ok, IndexTbl} = ets:file2tab(?INDEX_FILE),
            {DocTbl, IndexTbl};
        _Error ->
            get_docs(Heir)
    end.

reload({OldDocTbl, OldIndexTbl}, Heir) ->
    {DocTbl, IndexTbl} = get_docs(Heir),
    true = ets:delete(OldDocTbl),
    true = ets:delete(OldIndexTbl),
    {DocTbl, IndexTbl}.

get_docs(Heir) ->
    DocTbl = create_ets(noname, [ordered_set, public, {heir, Heir, doc_table}]),
    ok = scan_docs(DocTbl, "http://www.erlang.org/doc/man/erlang.html"),
    ok = scan_docs(DocTbl, "http://www.erlang.org/doc/man/array.html"),
    ok = scan_docs(DocTbl, "http://www.erlang.org/doc/man/application.html"),
    ok = scan_docs(DocTbl, "http://www.erlang.org/doc/man/crypto.html"),
    ok = scan_docs(DocTbl, "http://erlang.org/doc/man/inets.html"),
    IndexTbl = make_index(DocTbl, Heir),
    ok = ets:tab2file(DocTbl, ?DOC_FILE),
    ok = ets:tab2file(IndexTbl, ?INDEX_FILE),
    {DocTbl, IndexTbl}.

make_index(DocTable, Heir) ->
    IndexTable = create_ets(noname, [bag, public, {heir, Heir, index_table}]),
    [make_index(IndexTable, DocTable, N, ets:first(DocTable)) || N <- lists:seq(1,10)],
    lager:info("~p indexes created OK", [ets:info(IndexTable, size)]),
    IndexTable.

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
    case binary:split(MFA, <<":">>, [global]) of
        [Module, <<>>] -> 
            {Module, Module, none};
        [Module, _, FunAr] -> 
            [Fun, Arity] = binary:split(FunAr, <<"/">>),
            {Module, Fun, Arity};
        [Module, FunAr] -> 
            [Fun, Arity] = binary:split(FunAr, <<"/">>),
            {Module, Fun, Arity}
    end.

get_part(Bin, Size) ->
    case Size >= size(Bin) of
        true  -> Bin;
        false -> binary:part(Bin, {0, Size})
    end.

scan_docs(Table, Url) ->
    inets:start(),
    lager:info("start grabbing erlang docs at ~p", [Url]),
    {ok, {{_Proto, 200, "OK"}, _Headers, Body}} = httpc:request(Url),
    {_String, _Attributes, All} = mochiweb_html:parse(Body),
    {_, R1} = get_node(<<"body">>, All),
    {_, R2} = get_node(<<"div">>, <<"id">>, <<"container">>, R1),
    {_, R3} = get_node(<<"div">>, <<"id">>, <<"leftnav">>, R2),
    {_, R4} = get_node(<<"div">>, <<"class">>, <<"innertube">>, R3),
    {_, R5} = get_node(<<"ul">>,  <<"class">>, <<"flipMenu">>, R4),
    [ets:insert(Table, get_section(S)) || S <- get_top(R5), section_is_valid(S)],
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


section_is_valid({SectionName, _}) -> section_is_valid(binary:replace(SectionName, <<" ">>, <<>>));
section_is_valid(<<"erl_driver">>) -> false;
section_is_valid(<<"erl_nif">>) -> false;
section_is_valid(_) -> true.

get_section({SectionName, [_, {<<"ul">>, _, Tree}]}) -> 
    get_section(binary:replace(SectionName, <<" ">>, <<>>), [], Tree).

parse_ref(SectionName, [{<<"a">>, [{<<"href">>, Ref}], [Name]}]) -> 
    {<<SectionName/binary, ":", Name/binary>>, Ref}.

parse_top(SectionName, [{<<"a">>, [{<<"href">>, Ref}], [_]}]) -> 
    {<<SectionName/binary, ":">>, Ref}.

get_section(Name, Acc, []) -> 
    lager:info("~p functions in ~p section loaded OK", [length(Acc), Name]),
    lists:reverse(Acc);
get_section(Name, Acc, [{<<"li">>, Attr, Inner} | Rest]) ->
    case proplists:is_defined(<<"title">>, Attr) of
        true  -> get_section(Name, [parse_ref(Name, Inner) | Acc], Rest);
        false -> get_section(Name, [parse_top(Name, Inner) | Acc], Rest)
    end;
get_section(Name, Acc, [_Curr | Rest]) ->
    get_section(Name, Acc, Rest).


create_ets(Table, Options) ->
    case ets:info(Table, size) of
        undefined -> ets:new(Table, Options);
        _         -> ets:delete(Table), ets:new(Table, Options)
    end.
    