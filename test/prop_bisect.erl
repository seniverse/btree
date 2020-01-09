-module(prop_bisect).
-include_lib("proper/include/proper.hrl").

prop_bisect() ->
    ?FORALL(
       List,
       orderedlist(integer()),
       ?FORALL(
          Value,
          integer(),
          prop_bisect(Value, List)
      )).

prop_bisect_match() ->
    ?FORALL(
       List,
       orderedlist(integer()),
       ?FORALL(
          Value,
          oneof([0|List]),
          prop_bisect(Value, List)
      )).

prop_bisect(Value, List) ->
    {Start, End} = btree_utils:bisect(Value, list_to_tuple(List)),
    {Head, Rest} = lists:split(Start, List),
    {Mid, Tail} = lists:split(End-1-Start, Rest),

    lists:all(fun (X) -> X < Value end, Head)
        and lists:all(fun (X) -> X == Value end, Mid)
        and lists:all(fun (X) -> X > Value end, Tail).
