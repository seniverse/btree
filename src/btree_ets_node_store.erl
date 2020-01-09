-module(btree_ets_node_store).

-export([new/2, load/2, store/3, remove/2]).

new(Name, Options) ->
    {?MODULE,
     ets:new(
      Name,
      [set|Options])}.

load(Tab, Id) ->
    case ets:lookup(Tab, Id) of
        [] ->
            {error, not_found};
        [{Id, Value}] ->
            {ok, Value}
    end.

store(Tab, Id, Bin) ->
    true = ets:insert(Tab, {Id, Bin}),
    ok.

remove(Tab, Id) ->
    true = ets:delete(Tab, Id),
    ok.
