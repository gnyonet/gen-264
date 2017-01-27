% vim:et sw=4 ts=4:

-record(h264_model, {
    tree = gb_trees:empty(),
    stack = [],
    syntax = unknown
}).
