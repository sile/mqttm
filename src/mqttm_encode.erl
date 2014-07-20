%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(mqttm_encode).

-include("mqttm.hrl").
-include("mqttm_internal.hrl").

-compile([inline]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([encode/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec encode(mqttm:message()) -> iodata().
encode(M = #mqttm_connect{})    -> encode_connect(M);
encode(M = #mqttm_connack{})    -> encode_connack(M);
encode(M = #mqttm_publish{})    -> encode_publish(M);
encode(M = #mqttm_puback{})     -> encode_puback(M);
encode(M = #mqttm_pubrec{})     -> encode_pubrec(M);
encode(M = #mqttm_pubrel{})     -> encode_pubrel(M);
encode(M = #mqttm_pubcomp{})    -> encode_pubcomp(M);
encode(M = #mqttm_subscribe{})  -> encode_subscribe(M);
encode(M = #mqttm_suback{})     -> encode_suback(M);
encode(M = #mqttm_unsubscribe{})-> encode_unsubscribe(M);
encode(M = #mqttm_unsuback{})   -> encode_unsuback(M);
encode(M = #mqttm_pingreq{})    -> encode_pingreq(M);
encode(M = #mqttm_pingresp{})   -> encode_pingresp(M);
encode(M = #mqttm_disconnect{}) -> encode_disconnect(M);
encode(M)                       -> error(badarg, [M]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec encode_connect(#mqttm_connect{}) -> iodata().
encode_connect(M) ->
    Payload = encode_connect_payload(M),
    [?ENCODE_HEADER_FIRST(?MESSAGE_TYPE_CONNECT), encode_length(byte_size(Payload)), Payload].

-spec encode_connack(#mqttm_connack{}) -> iodata().
encode_connack(M) ->
    <<?MESSAGE_TYPE_CONNACK:4, 0:4, 2, 0, (M#mqttm_connack.return_code)>>.

-spec encode_publish(#mqttm_publish{}) -> iodata().
encode_publish(M) ->
    Payload =
        case M#mqttm_publish.message_id of
            undefined -> [<<?ENCODE_STRING(M#mqttm_publish.topic_name)>>, M#mqttm_publish.payload];
            MessageId -> [<<?ENCODE_STRING(M#mqttm_publish.topic_name), MessageId:16>>, M#mqttm_publish.payload]
        end,
    [?ENCODE_HEADER_FIRST(?MESSAGE_TYPE_PUBLISH, M#mqttm_publish.dup_flag, M#mqttm_publish.qos_level, M#mqttm_publish.retain_flag),
     encode_length(iolist_size(Payload)) |
     Payload].

-spec encode_puback(#mqttm_puback{}) -> iodata().
encode_puback(M) ->
    <<?MESSAGE_TYPE_PUBACK:4, 0:4, 2, (M#mqttm_puback.message_id):16>>.

-spec encode_pubrec(#mqttm_pubrec{}) -> iodata().
encode_pubrec(M) ->
    <<?MESSAGE_TYPE_PUBREC:4, 0:4, 2, (M#mqttm_pubrec.message_id):16>>.

-spec encode_pubrel(#mqttm_pubrel{}) -> iodata().
encode_pubrel(M) ->
    <<?MESSAGE_TYPE_PUBREL:4,
      ?ENCODE_FLAG(M#mqttm_pubrel.dup_flag),
      1:2,
      0:1,
      2,
      (M#mqttm_pubrel.message_id):16>>.

-spec encode_pubcomp(#mqttm_pubcomp{}) -> iodata().
encode_pubcomp(M) ->
    <<?MESSAGE_TYPE_PUBCOMP:4, 0:4, 2, (M#mqttm_pubcomp.message_id):16>>.

-spec encode_subscribe(#mqttm_subscribe{}) -> iodata().
encode_subscribe(M) ->
    Payload = [<<(M#mqttm_subscribe.message_id):16>> |
               [<<?ENCODE_STRING(TopicName), 0:6, ?ENCODE_QOS(TopicQos)>> ||
                   {TopicName, TopicQos} <- M#mqttm_subscribe.payload]],
    [?ENCODE_HEADER_FIRST(?MESSAGE_TYPE_SUBSCRIBE, M#mqttm_subscribe.dup_flag, 1),
     encode_length(iolist_size(Payload)) |
     Payload].

-spec encode_suback(#mqttm_suback{}) -> iodata().
encode_suback(M) ->
    Payload = [<<(M#mqttm_suback.message_id):16>> |
               [<<0:6, ?ENCODE_QOS(Qos)>> || Qos <- M#mqttm_suback.payload]],
    [?ENCODE_HEADER_FIRST(?MESSAGE_TYPE_SUBACK),
     encode_length(iolist_size(Payload)) |
     Payload].

-spec encode_unsubscribe(#mqttm_unsubscribe{}) -> iodata().
encode_unsubscribe(M) ->
    Payload = [<<(M#mqttm_unsubscribe.message_id):16>> |
               [<<?ENCODE_STRING(TopicName)>> || TopicName <- M#mqttm_unsubscribe.payload]],
    [?ENCODE_HEADER_FIRST(?MESSAGE_TYPE_UNSUBSCRIBE, M#mqttm_unsubscribe.dup_flag, 1),
     encode_length(iolist_size(Payload)) |
     Payload].

-spec encode_unsuback(#mqttm_unsuback{}) -> iodata().
encode_unsuback(M) ->
    <<?MESSAGE_TYPE_UNSUBACK:4, 0:4, 2, (M#mqttm_unsuback.message_id):16>>.

-spec encode_pingreq(#mqttm_pingreq{}) -> iodata().
encode_pingreq(_M) -> <<?MESSAGE_TYPE_PINGREQ:4, 0:4, 0:8>>.

-spec encode_pingresp(#mqttm_pingresp{}) -> iodata().
encode_pingresp(_M) -> <<?MESSAGE_TYPE_PINGRESP:4, 0:4, 0:8>>.

-spec encode_disconnect(#mqttm_disconnect{}) -> iodata().
encode_disconnect(_M) -> <<?MESSAGE_TYPE_DISCONNECT:4, 0:4, 0:8>>.

-spec encode_connect_payload(#mqttm_connect{}) -> binary().
encode_connect_payload(M) ->
    #mqttm_connect{will = Will} = M,
    <<
      ?ENCODE_STRING(M#mqttm_connect.protocol_name),
      (M#mqttm_connect.protocol_version):8,

      %% flags
      ?ENCODE_FLAG(M#mqttm_connect.username =/= undefined),
      ?ENCODE_FLAG(M#mqttm_connect.password =/= undefined),
      ?ENCODE_FLAG(Will =/= undefined andalso Will#mqttm_will.retain_flag),
      ?ENCODE_QOS(case Will =:= undefined of true -> 0; false -> Will#mqttm_will.qos_level end),
      ?ENCODE_FLAG(Will =/= undefined),
      ?ENCODE_FLAG(M#mqttm_connect.clean_session_flag),
      0:1, % reserved

      (M#mqttm_connect.keep_alive_timer):16,
      ?ENCODE_STRING(M#mqttm_connect.client_id),
      ?ENCODE_STRING_IF_DEFINED(Will, Will#mqttm_will.topic_name),
      ?ENCODE_STRING_IF_DEFINED(Will, Will#mqttm_will.message),
      ?ENCODE_STRING_IF_DEFINED(M#mqttm_connect.username),
      ?ENCODE_STRING_IF_DEFINED(M#mqttm_connect.password)
    >>.

-spec encode_length(0..16#FFFFFF7F) -> byte() | iodata().
encode_length(N) when N =< 16#7F      -> N;
encode_length(N) when N =< 16#3FFF    ->
    <<A:7, B:7>> = <<N:14>>,
    <<1:1, B:7, 0:1, A:7>>;
encode_length(N) when N =< 16#1FFFFF  ->
    <<A:7, B:7, C:7>> = <<N:21>>,
    <<1:1, C:7, 1:1, B:7, 0:1, A:7>>;
encode_length(N) when N =< 16#FFFFFFF ->
    <<A:7, B:7, C:7, D:7>> = <<N:28>>,
    <<1:1, D:7, 1:1, C:7, 1:1, B:7, 0:1, A:7>>;
encode_length(N) -> error(badarg, [N]).
