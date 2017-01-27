% vim:et sw=4 ts=4:

-module(h264_syntax).
-include("h264_syntax.hrl").

-export([nal/1, sps/1, pps/1, slice_header/1]).
-export([get_class/1, get_table/1]).
-export([get_nal_syntax/2]).

get_class({Class, _Table}) -> Class.
get_table({_Class, Table}) -> Table.

nal(Module) -> #h264_syntax{
    type = nal,
    table = [
        {forbidden_zero_bit,        Module:u(1)},
        {nal_ref_idc,               Module:u(1)},
        {nal_unit_type,             Module:u(1)}
    ]
}.

get_nal_syntax(Module, 1) -> slice_header(Module);
get_nal_syntax(Module, 5) -> slice_header(Module);
get_nal_syntax(Module, 7) -> sps(Module);
get_nal_syntax(Module, 8) -> pps(Module);
get_nal_syntax(Module, 9) -> aud(Module);
get_nal_syntax(_Module, _Nal_unit_type) -> #h264_syntax{}.

-define(VAL(Key, Fun), #h264_val{key=Key, fn=Module:Fun}).
-define(VEC(Key, Fun), #h264_vec{key=Key, fn=Module:Fun}).

sps(Module) -> #h264_syntax{
    type = sps,
    table = [
        ?VAL(profile_idc,               u(8)),
        ?VAL(constraint_set0_flag,      u(1)),
        ?VAL(constraint_set1_flag,      u(1)),
        ?VAL(constraint_set2_flag,      u(1)),
        ?VAL(constraint_set3_flag,      u(1)),
        ?VAL(constraint_set4_flag,      u(1)),
        ?VAL(constraint_set5_flag,      u(1)),
        ?VAL(reserved_zero_2bits,       u(2)),
        ?VAL(level_idc,                 u(8)),
        ?VAL(seq_parameter_set_id,      ue(v)),
        #h264_if{fn=fun(Model) -> value_in(h264_model:get(Model, profile_idc),
                                    [100, 110, 122, 244, 44, 83, 86, 118, 128, 138]) end,
        then=[
            ?VAL(chroma_format_idc,        ue(v)),
            #h264_if{fn=fun(Model) -> value_in(h264_model:get(Model, chroma_format_idc), [3]) end,
            then=[
                ?VAL(separate_colour_plane_flag,       u(1))
            ]},
            ?VAL(bit_depth_luma_minus8,                ue(v)),
            ?VAL(bit_depth_chroma_minus8,              ue(v)),
            ?VAL(qpprime_y_zero_transform_bypass_flag, u(1)),
            ?VAL(seq_scaling_matrix_present_flag,      u(1)),
            #h264_if{fn=fun(Model) -> value_in(h264_model:get(Model, seq_scaling_matrix_present_flag), [1]) end,
            then=[
                #h264_for{fn=fun(Model, Count) ->
                    Count < if_switch(h264_model:get(Model, chroma_format_idc) =/= 3, 8, 12) end,
                do=[
                    ?VEC(seq_scaling_list_present_flag,     u(1)),
                    #h264_if{fn=fun(Model) -> h264_model:get(Model, seq_scaling_matrix_present_flag, stack) =:= 1 end,
                    then=[
                        #h264_if{fn=fun(Model) -> h264_model:index(Model) < 6 end,
                        then=[
                            #h264_call{table=scaling_list(Module, {'ScalingList4x4', 16, 'UseDefaultScalingMatrix4x4Flag'})}
                        ],
                        else=[
                            #h264_call{table=scaling_list(Module, {'ScalingList8x8', 64, 'UseDefaultScalingMatrix8x8Flag'}),
                                        % loopのインデックスから8引いた添字に書き込みたいのでinit,termで調整
                                        init=fun(Model) -> h264_model:push_index(Model, h264_model:index(Model) - 6) end,
                                        term=fun(Model) -> h264_model:pop_index(Model) end
                            }
                        ]}
                    ]}
                ]}
            ]},
            ?VAL(log2_max_frame_num_minus4,             ue(v))
        ]}
    ]
}.

scaling_list(Module, {ScalingList, SizeOfScalingList, UseDefaultScalingMatrixFlag}) -> #h264_syntax{
    type = scaling_list,
    table = [
        #h264_for{init=fun(Model) -> h264_model:set(h264_model:set(Model, nextScale, 8), lastScale, 8) end,
            count = SizeOfScalingList,
        do=[
            #h264_if{fn=fun(Model) -> h264_model:get(Model, nextScale) =/= 0 end,
            then=[
                ?VEC(delta_scale,      se(v)),
                #h264_call{init=fun(Model) ->
                    NextScale = (h264_model:get(Model, lastScale) + h264_model:get(Model, delta_scale) + 256) rem 256,
                    M2 = h264_model:set(Model, nextScale, NextScale),
                    h264_model:set(M2, UseDefaultScalingMatrixFlag, h264_model:index(M2) =:= 0 andalso NextScale =:= 0, stack)
                end}
            ]},
            #h264_call{init=fun(Model) -> 
                NextScale = h264_model:get(Model, nextScale),
                ScalingListJ = if_switch(NextScale =:= 0, h264_model:get(Model, lastScale), NextScale),
                M2 = h264_model:set(Model, ScalingList, ScalingListJ, stack),
                h264_model:set(M2, lastScale, ScalingListJ)
            end}
        ]
        }
    ]
}.

pps(_Module) -> #h264_syntax{
    type = pps,
    table = [
    ]
}.

aud(_Modlue) -> #h264_syntax{
    type = aud,
    table = [
    ]
}.

slice_header(_Module) -> #h264_syntax{
    type = slice_header,
    table = [
    ]
}.

value_in(_, []) -> false;
value_in(Value, [Value|_]) -> true;
value_in(Value, [_|T]) -> value_in(Value, T).

if_switch(true, V, _) -> V;
if_switch(false, _, V) -> V.

