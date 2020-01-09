-module(bplustree).

-export([lookup/3, smallest/2, largest/2, insert/5]).

smallest(NodeId, Store) ->
    {ok, {Keys, Children}} = bplustree_node:load(Store, NodeId),
    case size(Keys) == size(Children) of
        true ->
            {element(1, Keys), element(1, Children)};
        false ->
            smallest(element(1, Children), Store)
    end.

largest(NodeId, Store) ->
    {ok, {Keys, Children}} = bplustree_node:load(Store, NodeId),
    Size = size(Children),
    case size(Keys) of
        Size ->
            {element(Size, Keys), element(Size, Children)};
        _ ->
            largest(element(Size, Children), Store)
    end.


lookup(Key, NodeId, Store) ->
    {ok, {Keys, Children}} = bplustree_node:load(Store, NodeId),
    case size(Keys) == size(Children) of
        true ->
            case btree_utils:bisect(Key, Keys) of
                {Start, End} when Start + 1 == End ->
                    {error, not_found};
                {Start, End} when Start + 2 == End ->
                    {ok, element(Start+1, Children)}
            end;
        false ->
            {_, End} = btree_utils:bisect(Key, Keys),
            lookup(Key, element(End, Children), Store)
    end.

insert(Key, Value, Root, M, Store) ->
    Node = node_insert(Key, Value, {{},{Root}}, M, Store),
    ok = btree_node_store:remove(Store, Root),
    case Node of
        {{}, {NodeId}} ->
            NodeId;
        _ ->
            {ok, NodeId} = bplustree_node:store(Store, Node),
            NodeId
    end.

node_insert(Key, Value, {Keys, Values}, _M, _Store) when size(Keys) == size(Values) ->
    {Start, End} = btree_utils:bisect(Key, Keys),
    End = Start + 1,
    Keys1 = erlang:insert_element(End, Keys, Key),
    Values1 = erlang:insert_element(End, Values, Value),
    {Keys1, Values1};
node_insert(Key, Value, {Keys, Children}, M, Store) ->
    {Start, End} = btree_utils:bisect(Key, Keys),
    ChildId = element(End, Children),
    {ok, Child} = bplustree_node:load(Store, ChildId),
    Node = node_insert(Key, Value, Child, M, Store),
    ok = btree_node_store:remove(Store, ChildId),

    case Node of
        {Keys1, _} when size(Keys1) == M ->
            {Key1, Left, Right} = node_split(Node),
            {ok, LeftId} = bplustree_node:store(Store, Left),
            {ok, RightId} = bplustree_node:store(Store, Right),

            {erlang:insert_element(End, Keys, Key1),
             erlang:insert_element(End, setelement(End, Children, RightId), LeftId)};
        {Keys1, _} when size(Keys1) < M ->
            {ok, NodeId} = bplustree_node:store(Store, Node),
            {Keys, setelement(End, Children, NodeId)}
    end.

node_split({Keys, Values}) when size(Keys) == size(Values) ->
    Size = size(Keys),
    Mid = Size div 2,
    {LeftKeys, [Key|_]=RightKeys} = lists:split(Mid, tuple_to_list(Keys)),
    {LeftValues, RightValues} = lists:split(Mid, tuple_to_list(Values)),
    Left = {list_to_tuple(LeftKeys), list_to_tuple(LeftValues)},
    Right = {list_to_tuple(RightKeys), list_to_tuple(RightValues)},
    {Key, Left, Right};
node_split({Keys, Children}) ->
    Size = size(Keys),
    Mid = Size div 2,
    {LeftKeys, [Key|RightKeys]} = lists:split(Mid, tuple_to_list(Keys)),
    {LeftChildren, RightChildren} = lists:split(Mid + 1, tuple_to_list(Children)),
    Left = {list_to_tuple(LeftKeys), list_to_tuple(LeftChildren)},
    Right = {list_to_tuple(RightKeys), list_to_tuple(RightChildren)},
    {Key, Left, Right}.
