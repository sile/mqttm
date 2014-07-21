%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(mqttm_decode).

-include("mqttm.hrl").
-include("mqttm_internal.hrl").

-compile([inline]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([decode/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec decode(binary()) -> {[mqttm:message()], binary()}.
decode(Bytes) ->
    decode(Bytes, []).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec decode(binary(), [mqttm:message()]) -> {[mqttm:message()], binary()}.
decode(Bytes0 = <<Type:4, Dup:1, Qos:2, Retain:1, Bytes1/binary>>, Acc) ->
    case get_payload(Bytes1) of
        partial           -> {lists:reverse(Acc), Bytes0};
        {Payload, Bytes2} ->
            Message =
                case Type of
                    ?MESSAGE_TYPE_CONNECT     -> decode_connect(Payload);
                    ?MESSAGE_TYPE_CONNACK     -> decode_connack(Payload);
                    ?MESSAGE_TYPE_PUBLISH     -> decode_publish(Dup, Qos, Retain, Payload);
                    ?MESSAGE_TYPE_PUBACK      -> decode_puback(Payload);
                    ?MESSAGE_TYPE_PUBREC      -> decode_pubrec(Payload);
                    ?MESSAGE_TYPE_PUBREL      -> decode_pubrel(Dup, Qos, Payload);
                    ?MESSAGE_TYPE_PUBCOMP     -> decode_pubcomp(Payload);
                    ?MESSAGE_TYPE_SUBSCRIBE   -> decode_subscribe(Dup, Qos, Payload);
                    ?MESSAGE_TYPE_SUBACK      -> decode_suback(Payload);
                    ?MESSAGE_TYPE_UNSUBSCRIBE -> decode_unsubscribe(Dup, Qos, Payload);
                    ?MESSAGE_TYPE_UNSUBACK    -> decode_unsuback(Payload);
                    ?MESSAGE_TYPE_PINGREQ     -> decode_pingreq(Payload);
                    ?MESSAGE_TYPE_PINGRESP    -> decode_pingresp(Payload);
                    ?MESSAGE_TYPE_DISCONNECT  -> decode_disconnect(Payload);
                    _                         -> error({unknown_message_type, Type})
                end,
            decode(Bytes2, [Message | Acc])
    end;
decode(Bytes, Acc) ->
    {lists:reverse(Acc), Bytes}.

-spec decode_connect(binary()) -> #mqttm_connect{}.
decode_connect(Payload0) ->
    {ProtocolName, Payload1} = decode_string(Payload0),
    case Payload1 of
        <<ProtocolVersion:8,
          UserNameFlag:1, PasswordFlag:1, WillRetainFlag:1, WillQos:2, WillFlag:1, CleanSessionFlag:1, _:1,
          KeepAliveTimer:16, Payload2/binary>> ->
            {ClientId, Payload3} = decode_string(Payload2),
            {Will, Payload4} = decode_will(Payload3, WillRetainFlag, WillQos, WillFlag),
            {UserName, Payload5} =
                case UserNameFlag of
                    0 -> {undefined, Payload4};
                    1 -> decode_string(Payload4)
                end,
            {Password, _Payload6} =
                case PasswordFlag of
                    0 -> {undefined, Payload5};
                    1 -> decode_string(Payload5)
                end,
            #mqttm_connect{
               protocol_name      = ProtocolName,
               protocol_version   = ProtocolVersion,
               clean_session_flag = CleanSessionFlag =:= 1,
               keep_alive_timer   = KeepAliveTimer,
               client_id          = ClientId,
               will               = Will,
               username           = UserName,
               password           = Password
              };
        _ ->
            error({too_short_connect_command_payload, Payload0})
    end.

-spec decode_connack(binary()) -> #mqttm_connack{}.
decode_connack(<<_Reserved:8, ReturnCode:8, _/binary>>) ->
    #mqttm_connack{return_code = ReturnCode};
decode_connack(Payload) ->
    error({insufficient_connack_payload, Payload}).

-spec decode_publish(0|1, mqttm:qos_level(), 0|1, binary()) -> #mqttm_publish{}.
decode_publish(DupFlag, QosLevel, RetainFlag, Payload0) when ?IS_QOS_LEVEL(QosLevel) ->
    {Topic, Payload1} = decode_string(Payload0),
    case QosLevel of
        0 ->
            #mqttm_publish{
               dup_flag    = DupFlag =:= 1,
               qos_level   = QosLevel,
               retain_flag = RetainFlag =:= 1,
               topic_name  = Topic,
               payload     = Payload1
              };
        _ ->
            case Payload1 of
                <<MessageId:16, Payload2/binary>> ->
                    #mqttm_publish{
                       dup_flag    = DupFlag =:= 1,
                       qos_level   = QosLevel,
                       retain_flag = RetainFlag =:= 1,
                       topic_name  = Topic,
                       message_id  = MessageId,
                       payload     = Payload2
                      };
                _ ->
                    error({too_short_connect_command_payload, Payload0})
            end
    end.

-spec decode_puback(binary()) -> #mqttm_puback{}.
decode_puback(<<MessageId:16, _/binary>>) ->
    #mqttm_puback{message_id = MessageId};
decode_puback(Payload) ->
    error({too_short_puback_command_payload, Payload}).

-spec decode_pubrec(binary()) -> #mqttm_pubrec{}.
decode_pubrec(<<MessageId:16, _/binary>>) ->
    #mqttm_pubrec{message_id = MessageId};
decode_pubrec(Payload) ->
    error({too_short_pubrec_command_payload, Payload}).

-spec decode_pubrel(0|1, mqttm:qos_level(), binary()) -> #mqttm_pubrel{}.
decode_pubrel(DupFlag, 1, <<MessageId:16, _/binary>>) ->
    #mqttm_pubrel{dup_flag = DupFlag =:= 1, message_id = MessageId};
decode_pubrel(_DupFlag, QosLevel, Payload) ->
    case QosLevel =:= 1 of
        true  -> error({too_short_pubrel_command_payload, Payload});
        false -> error({unexpected_qos_level, QosLevel})
    end.

-spec decode_pubcomp(binary()) -> #mqttm_pubcomp{}.
decode_pubcomp(<<MessageId:16, _/binary>>) ->
    #mqttm_pubcomp{message_id = MessageId};
decode_pubcomp(Payload) ->
    error({too_short_pubcomp_command_payload, Payload}).

-spec decode_subscribe(0|1, mqttm:qos_level(), binary()) -> #mqttm_subscribe{}.
decode_subscribe(DupFlag, 1, <<MessageId:16, Payload/binary>>) ->
    TopicList = map_binary(fun (Bin0) ->
                                   {Topic, Bin1} = decode_string(Bin0),
                                   case Bin1 of
                                       <<0:6, Qos:2, Bin2/binary>> -> {{Topic, Qos}, Bin2};
                                       _ -> error({too_short_subscribe_command_payload, Payload})
                                   end
                           end,
                           Payload),
    #mqttm_subscribe{
       dup_flag   = DupFlag =:= 1,
       message_id = MessageId,
       payload    = TopicList
      };
decode_subscribe(_DupFlag, QosLevel, Payload) ->
    case QosLevel =:= 1 of
        true  -> error({too_short_subscribe_command_payload, Payload});
        false -> error({unexpected_qos_level, QosLevel})
    end.

-spec decode_suback(binary()) -> #mqttm_suback{}.
decode_suback(<<MessageId:16, Payload/binary>>) ->
    QosList = [Qos || <<0:6, Qos:2>> <= Payload],
    #mqttm_suback{
       message_id = MessageId,
       payload    = QosList
      };
decode_suback(Payload) ->
    error({too_short_suback_command_payload, Payload}).

-spec decode_unsubscribe(0|1, mqttm:qos_level(), binary()) -> #mqttm_unsubscribe{}.
decode_unsubscribe(DupFlag, 1, <<MessageId:16, Payload/binary>>) ->
    TopicList = map_binary(fun decode_string/1, Payload),
    #mqttm_unsubscribe{
       dup_flag   = DupFlag =:= 1,
       message_id = MessageId,
       payload    = TopicList
      };
decode_unsubscribe(_DupFlag, QosLevel, Payload) ->
    case QosLevel =:= 1 of
        true  -> error({too_short_unsubscribe_command_payload, Payload});
        false -> error({unexpected_qos_level, QosLevel})
    end.

-spec decode_unsuback(binary()) -> #mqttm_unsuback{}.
decode_unsuback(<<MessageId:16, _/binary>>) ->
    #mqttm_unsuback{message_id = MessageId};
decode_unsuback(Payload) ->
    error({too_short_unsuback_command_payload, Payload}).

-spec decode_pingreq(binary()) -> #mqttm_pingreq{}.
decode_pingreq(_) ->
    #mqttm_pingreq{}.

-spec decode_pingresp(binary()) -> #mqttm_pingresp{}.
decode_pingresp(_) ->
    #mqttm_pingresp{}.

-spec decode_disconnect(binary()) -> #mqttm_disconnect{}.
decode_disconnect(_) ->
    #mqttm_disconnect{}.
        
-spec get_payload(binary()) -> partial | {binary(), binary()}.
get_payload(Bytes0) ->
    case decode_remaining_length(Bytes0) of
        partial          -> partial;
        {Length, Bytes1} ->
            case Bytes1 of
                <<Payload:Length/binary, Rest/binary>> ->
                    {Payload, Rest};
                _ ->
                    partial
            end
    end.

-spec decode_remaining_length(binary()) -> partial | {0..16#FFFFFFF, binary()}.
decode_remaining_length(<<0:1, A:7, Bytes/binary>>) ->
    {A, Bytes};
decode_remaining_length(<<1:1, A:7, 0:1, B:7, Bytes/binary>>) ->
    {(B bsl 7) bor A, Bytes};
decode_remaining_length(<<1:1, A:7, 1:1, B:7, 0:1, C:7, Bytes/binary>>) ->
    {(C bsl 7) bor (B bsl 7) bor A, Bytes};
decode_remaining_length(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 0:1, D:7, Bytes/binary>>) ->
    {(D bsl 7) bor (C bsl 7) bor (B bsl 7) bor A, Bytes};
decode_remaining_length(Bytes = <<_:24, 1:1, _:7, _/binary>>) ->
    error({too_large_remaining_length, Bytes});
decode_remaining_length(_) ->
    partial.

-spec decode_will(binary(), 0|1, mqttm:qos_level(), 0|1) -> {mqttm:will() | undefined, binary()}.
decode_will(Bytes, _RetainFlag, _QosLevel, 0) -> {undefined, Bytes};
decode_will(Bytes0, RetainFlag, QosLevel,  1) ->
    {Topic, Bytes1} = decode_string(Bytes0),
    {Message, Bytes2} = decode_string(Bytes1),
    Will = #mqttm_will{
              qos_level   = QosLevel,
              retain_flag = RetainFlag =:= 1,
              topic_name  = Topic,
              message     = Message
             },
    {Will, Bytes2}.

-spec decode_string(binary()) -> {binary(), binary()}.
decode_string(<<Size:16, Str:Size/binary, Rest/binary>>) ->
    {Str, Rest};
decode_string(Bytes) ->
    error({insufficient_string_bytes, Bytes}).

-spec map_binary(Fun, binary()) -> [term()] when
      Fun :: fun ((binary()) -> {term(), binary()}).
map_binary(Fun, Bin) ->
    map_binary(Fun, Bin, []).

-spec map_binary(Fun, binary(), [term()]) -> [term()] when
      Fun :: fun ((binary()) -> {term(), binary()}).
map_binary(_Fun, <<"">>, Acc) ->
    lists:reverse(Acc);
map_binary(Fun, Bin0, Acc) ->
    {X, Bin1} = Fun(Bin0),
    map_binary(Fun, Bin1, [X | Acc]).

