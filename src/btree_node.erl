-module(btree_node).

-export([load/2, store/2]).

load(_, []) ->
    {ok, {{}, {}, []}};
load(Store, NodeId) ->
    case btree_node_store:load(Store, NodeId) of
        {ok, Bin} ->
            {ok, binary_to_term(Bin, [safe])};
        {error, _} = Error ->
            Error
    end.

store(_, {{}, {}, []}) ->
    [];
store(Store, Node) ->
    Bin = term_to_binary(Node, [compressed]),
    NodeId = crypto:hash(sha512, Bin),
    ok = btree_node_store:store(Store, NodeId, Bin),
    {ok, NodeId}.
