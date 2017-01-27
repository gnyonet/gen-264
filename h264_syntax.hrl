% vim:et sw=4 ts=4:

-record(h264_syntax, {
    type = unknown,
    table = []
}).

-record(h264_val, {
    key = unknown,
    fn = fun(_Model, _Binary) -> {error, uninitialized_record} end
}).

% 配列として定義されているデータの定義
-record(h264_vec, {
    key = unknown,
    fn = fun(_Model, _Binary) -> {error, uninitialized_record} end
}).

-record(h264_if, {
    fn = fun(_Model) -> {error, uninitialized_record} end,
    then = [],
    else = []
}).

-record(h264_for, {
    init = fun(Model) -> Model end,
    fn = fun(_Model, _Index) -> {error, uninitialized_record} end,
    count = 0,
    do = []
}).

-record(h264_call, {
    init = fun(Model) -> Model end,
    table = [],
    term = fun(Model) -> Model end
}).

