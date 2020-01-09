-module(btree_node_store).

-export([load/2, store/3, remove/2]).

load({Mod, Arg}, Id) ->
    Mod:load(Arg, Id).

store({Mod, Arg}, Id, Bin) ->
    Mod:store(Arg, Id, Bin).

remove(_, []) ->
    ok;
remove({Mod, Arg}, Id) ->
    Mod:remove(Arg, Id).
