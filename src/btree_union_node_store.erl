-module(btree_union_node_store).

-export([new/2, load/2, store/3, remove/2]).

new(Upper, Lower) ->
    {?MODULE, {Upper, Lower}}.

load({Upper, Lower}, Id) ->
    case btree_node_store:load(Upper, Id) of
        {ok, Value} ->
            Value;
        {error, not_found} ->
            btree_node_store:load(Lower, Id)
    end.

store({Upper, _}, Id, Bin) ->
    btree_node_store:store(Upper, Id, {ok, Bin}).

remove({Upper, _}, Id) ->
    btree_node_store:store(Upper, Id, {error, not_found}).
