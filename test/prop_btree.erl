-module(prop_btree).
-include_lib("proper/include/proper.hrl").

prop_btree() ->
    ?FORALL(
       List,
       list(tuple([pos_integer(), integer()])),
       prop_btree(5, List)).

prop_btree(M, List) ->
    {_, Tab} = Store = btree_ets_node_store:new(btree, []),
    try
        prop_btree(List, [], M, Store)
    after
        ets:delete(Tab)
    end.

prop_btree([], _, _, _) ->
    true;
prop_btree([{K,V}|T], Root, M, Store) ->
    case btree:lookup(K, Root, Store) of
        {ok, _} ->
            prop_btree(T, Root, M, Store);
        {error, not_found} ->
            Root1 = btree:insert(K, V, Root, M, Store),
            prop_btree(Root1, M, Store) and prop_btree(T, Root1, M, Store)
    end.

prop_btree(NodeId, M, Store) ->
    {ok, Node} = btree_node:load(Store, NodeId),
    prop_btree_node(Node, M, Store).

prop_btree_node(Node, M, Store) ->
    case Node of
        {Keys, Values, []}
          when size(Keys) < M,
               size(Values) == size(Keys) ->
            prop_btree_keys(tuple_to_list(Keys));
        {Keys, Values, Children}
          when size(Keys) < M,
               size(Values) == size(Keys),
               size(Children) == size(Keys) + 1 ->

            prop_btree_keys(tuple_to_list(Keys)) and prop_btree_children([0] ++ tuple_to_list(Keys) ++ [infinity], tuple_to_list(Children), M, Store);
        _ ->
            false
    end.

prop_btree_children([infinity], [], _, _) ->
    true;
prop_btree_children([A,B|T1], [C|T2], M, Store) ->
    prop_btree_child(A, B, C, M, Store) and prop_btree_children([B|T1], T2, M, Store).

prop_btree_child(A, B, NodeId, M, Store) ->
    {ok, Node} = btree_node:load(Store, NodeId),
    {Keys, _, _} = Node,
    (size(Keys) * 2 + 1 >= M) 
        and (A < element(1, btree:smallest(NodeId, Store)))
        and (element(1, btree:largest(NodeId, Store)) < B)
        and prop_btree_node(Node, M, Store).

prop_btree_keys([]) ->
    true;
prop_btree_keys([_]) ->
    true;
prop_btree_keys([A,B|T]) when A < B ->
    prop_btree_keys([B|T]).
