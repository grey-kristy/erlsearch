-module(es_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% API

-export([search/1, get_docs/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    es_scan:load(),
    {ok, {}}.

%% API

search(Request) -> gen_server:call(?MODULE, {search, Request}).

get_docs() -> gen_server:call(?MODULE, {get_docs}).


%% Server

handle_call({search, Request}, _From, State) -> 
    {reply, es_scan:search(Request), State};
handle_call({get_docs}, _From, State) -> 
    {reply, es_scan:get_docs(), State}.


handle_info(_Msg, State) -> 
    {noreply, State}.


handle_cast(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason, _State) -> ok.
