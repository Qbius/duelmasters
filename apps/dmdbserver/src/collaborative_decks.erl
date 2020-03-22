-module(collaborative_decks).
-behaviour(gen_server).

-export([host/2, join/1, move/3, sync/2, drop/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

init(_Args) ->
    {ok, #{}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

host(Deckname, Deckcode) ->
    gen_server:call(?MODULE, {host, Deckname, Deckcode}).

join(Deckname) ->
    gen_server:call(?MODULE, {join, Deckname}).

move(Deckname, Updatestr, UpdatedDeckcode) ->
    gen_server:call(?MODULE, {move, Deckname, Updatestr, UpdatedDeckcode}).

sync(Deckname, UpdatedDeckcode) ->
    gen_server:call(?MODULE, {sync, Deckname, UpdatedDeckcode}).

drop(Deckname) ->
    gen_server:call(?MODULE, {drop, Deckname}).

handle_call({host, Deckname, Deckcode}, {From, _}, State) ->
    {reply, ok, State#{Deckname => {[From], Deckcode}}};
handle_call({join, Deckname}, {From, _}, State) ->
    case State of
        #{Deckname := {Connected, Deckcode}} ->
            {reply, Deckcode, State#{Deckname => {[From | Connected], Deckcode}}};
        _ ->
            {reply, notfound, State}
    end;
handle_call({move, Deckname, Updatestr, UpdatedDeckcode}, {From, _}, State) ->
    case State of
        #{Deckname := {Connected, _Deckcode}} ->
            lists:foreach(fun(PID) ->
                io:format("~nSending to ~p", [PID]),
                PID ! {move, Updatestr}
            end, Connected -- [From]),
            {reply, ok, State#{Deckname => {Connected, UpdatedDeckcode}}};
        _ ->
            {reply, notfound, State}
    end;
handle_call({sync, Deckname, SyncedDeckcode}, _From, State) ->
    case State of
        #{Deckname := {Connected, _Deckcode}} ->
            {reply, ok, State#{Deckname => {Connected, SyncedDeckcode}}};
        _ ->
            {reply, notfound, State}
    end;
handle_call({drop, Deckname}, {From, _}, State) ->
    case State of
        #{Deckname := {Connected, Deckcode}} ->
            {reply, ok, State#{Deckname => {Connected -- [From], Deckcode}}};
        _ ->
            {reply, notfound, State}
    end.


handle_cast(_, State) ->
    io:fwrite("Unrecognized message in collaborative_decks gen server"),
    {noreply, State}.