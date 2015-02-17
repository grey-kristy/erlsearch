-module(es_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% API

-export([search/1, reload/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, es_scan:load(self())}.

%% API

search(Request) -> gen_server:call(?MODULE, {search, Request}).

reload() -> gen_server:call(?MODULE, {reload}, 30000).


%% Server

handle_call({search, Request}, _From, State) -> 
    {reply, es_scan:search(State, Request), State};
handle_call({reload}, _From, State) -> 
    spawn(es_scan, reload, [State, self()]),
    {reply, ok, State}.


handle_info({'ETS-TRANSFER', DocTable, _From, doc_table}, {_, IndexTable}) -> 
    {noreply, {DocTable, IndexTable}};
handle_info({'ETS-TRANSFER', IndexTable, _From, index_table}, {DocTable, _}) -> 
    {noreply, {DocTable, IndexTable}};
handle_info(Msg, State) -> 
    lager:warn("es_server is recived unexpected message: ~p", [Msg]),
    {noreply, State}.


handle_cast(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason, _State) -> ok.

