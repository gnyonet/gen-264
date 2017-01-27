% vim:et sw=4 ts=4:

-module(h264_serializer).

-export(u/1,ue/1).

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

count_zero(<<>>, _) ->
    {error, to_small_data};
count_zero(<<0:1,Rest/bitstring>>, ACC) ->
    count_zero(Rest, ACC+1);
count_zero(Binary = <<1:1,_/bitstring>>, ACC) ->
    {ok, {ACC, Binary}}.

