-module(cache).

-behavior(gen_server).

-export([start_link/0, add/2, init/1, lookup/1, lookup_by_date/2, handle_call/3,
         handle_cast/2]).

-define(EXPIRE_AT_SECS, 60).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    ets:new(cache, [named_table, private]),
    {ok, []}.

add(Key, Value) ->
    gen_server:cast(?MODULE, {add, Key, Value}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

lookup_by_date(From, To) ->
    gen_server:call(?MODULE, {lookup_by_date, From, To}).

handle_cast({add, Key, Value}, State) ->
    Now = calendar:universal_time(),
    ExpireAtSecs = calendar:datetime_to_gregorian_seconds(Now) + ?EXPIRE_AT_SECS,
    ets:insert(cache, {Key, {Value, ExpireAtSecs}}),
    {noreply, State}.

handle_call({lookup, Key}, _, State) ->
    Result =
        case ets:lookup(cache, Key) of
            [{_, {Value, ExpireAtSecs}}] ->
                Now = calendar:universal_time(),
                NowInSeconds = calendar:datetime_to_gregorian_seconds(Now),
                if NowInSeconds > ExpireAtSecs ->
                       ets:delete(cache, Key),
                       undefined;
                   true ->
                       Value
                end;
            _ ->
                undefined
        end,
    {reply, Result, State};
handle_call({lookup_by_date, From, To}, _, State) ->
    FromSeconds = calendar:datetime_to_gregorian_seconds(From),
    ToSeconds = calendar:datetime_to_gregorian_seconds(To),
    RawData =
        ets:select(cache,
                   [{{'$1', {'$2', '$3'}},
                     [{'andalso', {'>=', '$3', FromSeconds}, {'<', '$3', ToSeconds}}],
                     [{{'$1', '$2'}}]}]),

    Results = lists:map(fun({Key, Value}) -> #{key => Key, value => Value} end, RawData),
    {reply, Results, State}.
