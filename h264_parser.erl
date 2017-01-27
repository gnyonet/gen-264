% vim:et sw=4 ts=4:

-module(h264_parser).
-include("h264_syntax.hrl").
-include("h264_model.hrl").

-export([u/1,ue/1,se/1]).
-export([run/3, run_file/2, run_device/3, run_binary/2]).

u(V) ->
    fun(<<Value:V, Rest/bitstring>>) ->
        {ok, {Value, Rest}}
    end.

ue(v) ->
    fun(Binary) ->
        {ok, {Count, Rest}} = count_zero(Binary, 0),
        {ok, {Value, Rest2}} = (u(Count+1))(Rest),
        {ok, {Value-1, Rest2}}
    end.
se(v) ->
    fun(Binary) ->
        V = (ue(v))(Binary),
        -1 * (V band 1) * (V bsr 1)
    end.

run_file(FileName, Model) when is_list(FileName) ->
    {ok, IoDevice} = file:open(FileName, [read, binary]),
    run_device(IoDevice, <<>>, Model).

run_device(IoDevice, Rest, Model) ->
    case file:read(IoDevice, 1024 - byte_size(Rest)) of
        {ok, Data} ->
            {ok, Rest2} = run(h264_syntax:nal(), <<Rest/binary,Data/binary>>, Model),
            run(IoDevice, Rest2, Model);
        eof -> ok
    end.

run_binary(Binary, Model = #h264_model{syntax=unknown}) ->
    case find_start_code(Binary) of
        {ok, _} -> Model#h264_model{syntax=annexb};
        _ -> Model#h264_model{syntax=mp4}
    end;
run_binary(Binary, Model = #h264_model{syntax=annexb}) ->
    case find_start_code(Binary) of
        {ok, NalBody} -> run(h264_syntax:nal(h264_parser), NalBody, Model);
        {not_found, Rest} -> {Model, Rest}
    end;
run_binary(_Binary, _Model = #h264_model{syntax=mp4}) ->
    ok.

run([], Binary, Model) -> {ok, Model, Binary};
run([#h264_val{key=Key, fn=Fun}|T], Binary, Model) ->
    {ok, {Value, Rest}} = Fun(Binary),
    io:format("~p = ~p~n", [Key, Value]),
    run(T, Rest, h264_model:set(Model, Key, Value));
run([#h264_vec{key=Key, fn=Fun}|T], Binary, Model) ->
    {ok, {Value, Rest}} = Fun(Binary),
    io:format("~p = ~p~n", [Key, Value]),
    run(T, Rest, h264_model:set(Model, Key, Value, stack));
run([#h264_if{fn=Fun, then=Then, else=Else}|T], Binary, Model) ->
    {ok, Model2, Rest} = case Fun(Model) of
        true -> run(Then, Binary, Model);
        false -> run(Else, Binary, Model)
    end,
    run(T, Rest, Model2);
run([#h264_for{init=Init, fn=Fun, count=Count, do=Do}|T], Binary, Model) ->
    Model2 = Init(Model),
    {ok, Model3, Rest} = run_loop({Fun, Count, Do}, Binary, Model2, 0, Fun(Model2, 0)),
    run(T, Rest, Model3);
run([#h264_call{init=Init, table=Table, term=Term}|T], Binary, Model) ->
    Model2 = Init(Model),
    {ok, Model3, Rest} = run(Table, Binary, Model2),
    run(T, Rest, Term(Model3)).

run_loop({Fun, Count, Do}, Binary, Model, Index, true) when Count =:= 0 orelse Index < Count ->
    {ok, Model2, Rest} = run(Do, Binary, h264_model:push_index(Model, Index)),
    Model3 = h264_model:pop_index(Model2),
    run_loop({Fun, Count, Do}, Rest, Model3, Index+1, Fun(Model3, Index+1));
run_loop({_Fun, _Count, _Do}, Binary, Model, _Index, _Cond) ->
    {ok, Model, Binary}.
    
find_start_code(Binary) when byte_size(Binary) < 3 ->
    {not_found, Binary};
find_start_code(<<0,0,0,Rest/bitstring>>) ->
	find_start_code(<<0,0,Rest/bitstring>>);
find_start_code(Binary = <<0,0,1,_/bitstring>>) ->
    {ok, Binary};
find_start_code(<<_, Rest/bitstring>>) ->
    find_start_code(Rest).

count_zero(<<>>, _) ->
    {error, to_small_data};
count_zero(<<0:1,Rest/bitstring>>, ACC) ->
    count_zero(Rest, ACC+1);
count_zero(Binary = <<1:1,_/bitstring>>, ACC) ->
    {ok, {ACC, Binary}}.

