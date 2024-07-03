-module(simple_cache_handler).

-behavior(cowboy_handler).

-export([init/2, str_to_datetime/1]).

str_to_datetime(Str) ->
    DateRegex =
        "^([0-9]+)/([0-9]{1,2})/([0-9]{1,2}) *([0-9]{1,2}):([0-9]{1,2}):([0-9"
        "]{1,2})$",
    case re:run(Str, DateRegex) of
        {match, [_ | Matches]} ->
            [Year, Month, Day, Hours, Mins, Seconds] =
                lists:map(fun({Start, Stop}) ->
                             {Value, _} =
                                 string:to_integer(
                                     string:sub_string(Str, Start + 1, Start + Stop)),
                             Value
                          end,
                          Matches),
            {{Year, Month, Day}, {Hours, Mins, Seconds}};
        _ ->
            error
    end.

success_reply(Result, Req) ->
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(Result),
                     Req).

handle(#{<<"action">> := <<"insert">>,
         <<"key">> := Key,
         <<"value">> := Value},
       Req) ->
    cache:add(Key, Value),
    success_reply(#{result => ok}, Req);
handle(#{<<"action">> := <<"lookup">>, <<"key">> := Key}, Req) ->
    success_reply(#{result => cache:lookup(Key)}, Req);
handle(#{<<"action">> := <<"lookup_by_date">>,
         <<"date_from">> := From,
         <<"date_to">> := To},
       Req) ->
    FromDatetime = str_to_datetime(binary_to_list(From)),
    ToDatetime = str_to_datetime(binary_to_list(To)),
    if (FromDatetime == error) or (ToDatetime == error) ->
           cowboy_req:reply(400, #{}, "Bad request", Req);
       true ->
           Data = cache:lookup_by_date(FromDatetime, ToDatetime),
           success_reply(#{result => Data}, Req)
    end;
handle(_, Req) ->
    cowboy_req:reply(404, Req).

init(Req0, State) ->
    {ok, Data, _} = cowboy_req:read_body(Req0),
    JasonData = jsx:decode(Data, []),
    Req = handle(JasonData, Req0),
    {ok, Req, State}.
