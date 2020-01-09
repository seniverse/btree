-module(btree).

-export([lookup/3, smallest/2, largest/2, insert/5]).


smallest(NodeId, Store) ->
    {ok, {Keys, Values, Children}} = bplustree_node:load(Store, NodeId),
    case Children of
        [] ->
            {element(1, Keys), element(1, Values)};
        _ ->
            smallest(element(1, Children), Store)
    end.

largest(NodeId, Store) ->
    {ok, {Keys, Values, Children}} = bplustree_node:load(Store, NodeId),
    case Children of
        [] ->
            Size = size(Keys),
            {element(Size, Keys), element(Size, Values)};
        _ ->
            largest(element(size(Children), Children), Store)
    end.


lookup(Key, NodeId, Store) ->
    {ok, {Keys, Values, Children}} = btree_node:load(Store, NodeId),
    case btree_utils:bisect(Key, Keys) of
        {Start, End} when End == Start + 1 ->
            case Children of
                [] ->
                    {error, not_found};
                _ ->
                    lookup(Key, element(End, Children), Store)
            end;
        {Start, End} when End == Start + 2 ->
            {ok, element(Start+1, Values)}
    end.

insert(Key, Value, Root, M, Store) ->
    Node = node_insert(Key, Value, {{},{},{Root}}, M, Store),
    ok = btree_node_store:remove(Store, Root),
    case Node of
        {{}, {}, {NodeId}} ->
            NodeId;
        _ ->
            {ok, NodeId} = btree_node:store(Store, Node),
            NodeId
    end.

node_insert(Key, Value, {Keys, Values, []}, _M, _Store) ->
    {Start, End} = btree_utils:bisect(Key, Keys),
    End = Start + 1,
    Keys1 = erlang:insert_element(End, Keys, Key),
    Values1 = erlang:insert_element(End, Values, Value),
    {Keys1, Values1, []};
node_insert(Key, Value, {Keys, Values, Children}, M, Store) ->
    {Start, End} = btree_utils:bisect(Key, Keys),
    End = Start + 1,
    ChildId = element(End, Children),
    {ok, Child} = btree_node:load(Store, ChildId),
    Node = node_insert(Key, Value, Child, M, Store),
    ok = btree_node_store:remove(Store, ChildId),
    case Node of
        {Keys1, _, _} when size(Keys1) == M ->
            {Key1, Value1, Left, Right} = node_split(Node),
            {ok, LeftId} = btree_node:store(Store, Left),
            {ok, RightId} = btree_node:store(Store, Right),

            {erlang:insert_element(End, Keys, Key1),
             erlang:insert_element(End, Values, Value1),
             erlang:insert_element(End, setelement(End, Children, RightId), LeftId)};
        {Keys1, _, _} when size(Keys1) < M ->
            {ok, NodeId} = btree_node:store(Store, Node),
            {Keys, Values, setelement(End, Children, NodeId)}
    end.


node_split({Keys, Values, Children}) ->
    Size = size(Keys),
    Mid = Size div 2,
    {LeftKeys, [Key|RightKeys]} = lists:split(Mid, tuple_to_list(Keys)),
    {LeftValues, [Value|RightValues]} = lists:split(Mid, tuple_to_list(Values)),
    case Children of
        [] ->
            Left = {list_to_tuple(LeftKeys), list_to_tuple(LeftValues), []},
            Right = {list_to_tuple(RightKeys), list_to_tuple(RightValues), []};
        _ ->
            {LeftChildren, RightChildren} = lists:split(Mid + 1, tuple_to_list(Children)),
            Left = {list_to_tuple(LeftKeys), list_to_tuple(LeftValues), list_to_tuple(LeftChildren)},
            Right = {list_to_tuple(RightKeys), list_to_tuple(RightValues), list_to_tuple(RightChildren)}
    end,
    {Key, Value, Left, Right}.
