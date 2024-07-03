-module(list_utils).

-export([compress/1, compress_no_tail/1, dedup_no_tail/1, dedup/1]).


compress([], _, Acc) ->
    lists:reverse(Acc);
compress([H | Tail], Set, Acc) ->
    case sets:is_element(H, Set) of
        true ->
            compress(Tail, Set, Acc);
        false ->
            compress(Tail, sets:add_element(H, Set), [H | Acc])
    end.

compress([]) ->
    [];
compress(List) ->
    compress(List, sets:new(), []).

compress_no_tail([], _) ->
    [];
compress_no_tail([H | Tail], Set) ->
    case sets:is_element(H, Set) of
        true ->
            compress_no_tail(Tail, Set);
        false ->
            [H | compress_no_tail(Tail, sets:add_element(H, Set))]
    end.

compress_no_tail([]) ->
    [];
compress_no_tail(List) ->
    compress_no_tail(List, sets:new()).

dedup_no_tail([], _) ->
    [];
dedup_no_tail([H | Tail], H) ->
    dedup_no_tail(Tail, H);
dedup_no_tail([H | Tail], _) ->
    [H | dedup_no_tail(Tail, H)].

dedup_no_tail([]) ->
    [];
dedup_no_tail([H | _] = List) ->
    [H | dedup_no_tail(List, H)].

dedup([], Acc) ->
    lists:reverse(Acc);
dedup([H | Tail], []) ->
    dedup(Tail, [H]);
dedup([H | Tail], [H | _] = Acc) ->
    dedup(Tail, Acc);
dedup([H | Tail], Acc) ->
    dedup(Tail, [H | Acc]).

dedup(List) ->
    dedup(List, []).
