-module(bouncer_thrift).

%% API
-export([to_thrift_struct/3]).

-type struct_flavour() :: struct | exception | union.
-type field_num() :: pos_integer().
-type field_name() :: atom().
-type field_req() :: required | optional | undefined.

-type type_ref() :: {module(), atom()}.
-type field_type() ::
    bool | byte | i16 | i32 | i64 | string | double |
    {enum, type_ref()} |
    {struct, struct_flavour(), type_ref()} |
    {list, field_type()} |
    {set, field_type()} |
    {map, field_type(), field_type()}.

-type struct_field_info() ::
    {field_num(), field_req(), field_type(), field_name(), any()}.

-spec to_thrift_struct([struct_field_info()], map(), tuple()) -> tuple().
to_thrift_struct([{Idx, _Req, Type, Name, Default} | Rest], Map, Acc) ->
    case maps:take(Name, Map) of
        {V, MapLeft} ->
            Acc1 = erlang:setelement(Idx + 1, Acc, to_thrift_value(Type, V)),
            to_thrift_struct(Rest, MapLeft, Acc1);
        error when Default /= undefined ->
            Acc1 = erlang:setelement(Idx + 1, Acc, Default),
            to_thrift_struct(Rest, Map, Acc1);
        error ->
            to_thrift_struct(Rest, Map, Acc)
    end;
to_thrift_struct([], MapLeft, Acc) ->
    case map_size(MapLeft) of
        0 ->
            Acc;
        _ ->
            throw({?MODULE, {excess_context_data, MapLeft}})
    end.

to_thrift_value({struct, struct, {Mod, Name}}, V = #{}) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    Acc = erlang:make_tuple(length(StructDef) + 1, undefined, [{1, Mod:record_name(Name)}]),
    to_thrift_struct(StructDef, V, Acc);
to_thrift_value({set, Type}, Vs) ->
    ordsets:from_list([to_thrift_value(Type, V) || V <- ordsets:to_list(Vs)]);
to_thrift_value(string, V) ->
    V;
to_thrift_value(i64, V) ->
    V;
to_thrift_value(i32, V) ->
    V;
to_thrift_value(i16, V) ->
    V;
to_thrift_value(byte, V) ->
    V.
