-module(btree_utils).

-export([bisect/2]).

bisect(Key, Tuple) ->
    bisect(0, size(Tuple) + 1, Key, Tuple).

bisect(Start, End, Key, Tuple) when Start + 1 < End ->
    Mid = (Start + End) div 2,
    case element(Mid, Tuple) of
        Key ->
            {Start1, _} = bisect_left(Start, Mid, Key, Tuple),
            {_, End1} = bisect_right(Mid, End, Key, Tuple),
            {Start1, End1};
        Key1 when Key1 < Key ->
            bisect(Mid, End, Key, Tuple);
        Key2 when Key < Key2 ->
            bisect(Start, Mid, Key, Tuple)
    end;
bisect(Start, End, _, _) ->
    {Start, End}.

bisect_left(Start, End, Key, Tuple) when Start + 1 < End->
    Mid = (Start + End) div 2,
    case element(Mid, Tuple) of
        Key1 when Key1 < Key ->
            bisect_left(Mid, End, Key, Tuple);
        Key ->
            bisect_left(Start, Mid, Key, Tuple)
    end;
bisect_left(Start, End, _, _) ->
    {Start, End}.

bisect_right(Start, End, Key, Tuple) when Start + 1 < End ->
    Mid = (Start + End) div 2,
    case element(Mid, Tuple) of
        Key2 when Key2 > Key ->
            bisect_right(Start, Mid, Key, Tuple);
        Key ->
            bisect_right(Mid, End, Key, Tuple)
    end;
bisect_right(Start, End, _, _) ->
    {Start, End}.
