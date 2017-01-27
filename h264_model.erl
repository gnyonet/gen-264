% vim:et sw=4 ts=4:

-module(h264_model).
-include("h264_model.hrl").

-export([new/0, class/2, class/3, get/2, get/3, set/3, set/4, index/1, push_index/2, pop_index/1]).

new() -> #h264_model{}.

class(Name, Key) -> {Name, Key}.
class(Name, Index, Key) -> {Name, Index, Key}.

get(Model, Key) -> gb_trees:get(Key, Model#h264_model.tree).

get(Model, Key, stack) -> get(Model, Key, Model#h264_model.stack);
get(Model, Key, Stack) -> get(Model, {Key, Stack}).

set(Model, Key, Value) -> Model#h264_model{tree=gb_trees:insert(Key, Value, Model#h264_model.tree)}.

set(Model, Key, Value, stack) -> set(Model, Key, Value, Model#h264_model.stack);
set(Model, Key, Value, Stack) -> set(Model, {Key, Stack}, Value).

push_index(Model, Index) -> Model#h264_model{stack=[Index|Model#h264_model.stack]}.
pop_index(Model) ->
    [_|T] = Model#h264_model.stack,
    Model#h264_model{stack = T}.

index(#h264_model{stack=[H|_]}) -> H.


