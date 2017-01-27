% vim:et sw=4 ts=4:
-module(gen_264).
-export([read_file/1]).



read_syntax(Table, Binary) ->
    read_syntax(Table, Binary, gb_trees:empty()).
read_syntax([], Binary, Model) -> {ok, Model, Binary};
read_syntax([{Key, Fun}|T], Binary, Model) ->
    {ok, {Value, Rest}} = Fun(Binary),
    io:format("~p = ~p~n", [Key, Value]),
    read_syntax(T, Rest, gb_trees:insert(Key, Value, Model));
read_syntax([{vector, Key, Fun}|T], Binary, Model) ->
    {ok, {Value, Rest}} = Fun(Binary),
    io:format("~p = ~p~n", [Key, Value]),
    List = gb_trees:get(Key, Model),
    read_syntax(T, Rest, gb_trees:insert(Key, List ++ [Value], Model));
read_syntax([{if_true, Fun, Table}|T], Binary, Model) ->
    {ok, Model2, Rest} = case Fun(Model) of
        true -> read_syntax(Table, Binary, Model);
        _ -> {ok, Model, Binary}
    end,
    read_syntax(T, Rest, Model2);
read_syntax([{if_both, Fun, TableTrue, TableFalse}|T], Binary, Model) ->
    {ok, Model2, Rest} = case Fun(Model) of
        true -> read_syntax(TableTrue, Binary, Model);
        false -> read_syntax(TableFalse, Binary, Model)
    end,
    read_syntax(T, Rest, Model2);
read_syntax([{loop, Fun, Table}|T], Binary, Model) ->
    {ok, Model2, Rest} = loop_run({Fun, Table}, Binary, Model, 0),
    read_syntax(T, Rest, Model2).

loop_run({Fun, Table}, Binary, Model, Count) ->
    case Fun(Model, Count) of
        true ->
            {ok, Model2, Rest} = read_syntax(Table, Binary, Model),
            loop_run({Fun, Table}, Rest, Model2, Count+1);
        _ -> {ok, Model, Binary}
    end.


sps_table() ->
pps_table() ->
    [
    ].

read_nal_unit(1, _Binary) -> ok;
read_nal_unit(5, _Binary) -> ok;
read_nal_unit(7, Binary) ->    % sps
    io:format("sps found~n"),
    read_syntax(sps_table(), Binary),
    ok;
read_nal_unit(8, Binary) ->    % pps
    io:format("pps found~n"),
    read_syntax(pps_table(), Binary),
    ok;
read_nal_unit(_, _) -> ok.  % unknown nal unit

read_nal_unit(<<_Forbidden_zero_bit:1, _Nal_ref_idc:2, Nal_unit_type:5, Rest/bitstring>>) ->
    %io:format("nal_ref_idc: ~p nal_unit_type:~p~n", [Nal_ref_idc, Nal_unit_type]),
    read_nal_unit(Nal_unit_type, Rest),
    case find_start_code(Rest) of
        {ok, <<0,0,1,Binary/bitstring>>} -> read_nal_unit(Binary);
        {not_found, Binary} -> {ok, Binary}
    end.

read_binary(Binary) ->
    case find_start_code(Binary) of
        {ok, <<0,0,1,Binary2/bitstring>>} -> read_nal_unit(Binary2);
        {not_found, Binary2} -> {ok, Binary2}
    end.

read_file(IoDevice, Rest) ->
    case file:read(IoDevice, 1024 - byte_size(Rest)) of
        {ok, Data} ->
            {ok, Rest2} = read_binary(<<Rest/binary,Data/binary>>),
            read_file(IoDevice, Rest2);
        eof -> ok
    end.
read_file(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read,binary]),
    read_file(IoDevice, <<>>).

