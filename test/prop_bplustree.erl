-module(prop_bplustree).
-include_lib("proper/include/proper.hrl").

prop_bplustree() ->
    ?FORALL(
       List,
       list(tuple([pos_integer(), integer()])),
       prop_bplustree(5, List)).

prop_bplustree(M, List) ->
    {_, Tab} = Store = btree_ets_node_store:new(bplustree, []),
    try
        prop_bplustree(List, [], M, Store)
    after
        ets:delete(Tab)
    end.

prop_bplustree([], _, _, _) ->
    true;
prop_bplustree([{K,V}|T], Root, M, Store) ->
    case bplustree:lookup(K, Root, Store) of
        {ok, _} ->
            prop_bplustree(T, Root, M, Store);
        {error, not_found} ->
            Root1 = bplustree:insert(K, V, Root, M, Store),
            prop_bplustree(Root1, M, Store) and prop_bplustree(T, Root1, M, Store)
    end.

prop_bplustree(NodeId, M, Store) ->
    {ok, Node} = bplustree_node:load(Store, NodeId),
    prop_bplustree_node(Node, M, Store).

prop_bplustree_node(Node, M, Store) ->
    case Node of
        {Keys, Values}
          when size(Keys) < M,
               size(Values) == size(Keys) ->
            prop_bplustree_keys(tuple_to_list(Keys));
        {Keys, Children}
          when size(Keys) < M,
               size(Children) == size(Keys) + 1 ->
            prop_bplustree_keys(tuple_to_list(Keys)) and prop_bplustree_children([0] ++ tuple_to_list(Keys) ++ [infinity], tuple_to_list(Children), M, Store);
        _ ->
            false
    end.

prop_bplustree_children([infinity], [], _, _) ->
    true;
prop_bplustree_children([A,B|T1], [C|T2], M, Store) ->
    prop_bplustree_child(A, B, C, M, Store) and prop_bplustree_children([B|T1], T2, M, Store).

prop_bplustree_child(A, B, NodeId, M, Store) ->
    {ok, Node} = bplustree_node:load(Store, NodeId),
    {Keys, _} = Node,
    (size(Keys) * 2 + 1 >= M)
        and ((A == 0) or (element(1, bplustree:smallest(NodeId, Store)) == A))
        and (element(1, bplustree:largest(NodeId, Store)) < B)
        and prop_bplustree_node(Node, M, Store).

prop_bplustree_keys([]) ->
    true;
prop_bplustree_keys([_]) ->
    true;
prop_bplustree_keys([A,B|T]) when A < B ->
    prop_bplustree_keys([B|T]).
