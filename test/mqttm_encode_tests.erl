%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
-module(mqttm_encode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("mqttm.hrl").
-include("mqttm_internal.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(FIXED_HEADER(Type, Dup, QoS, Retain, Length),
        (<<Type:4, Dup:1, QoS:2, Retain:1, Length:8>>)).

%%------------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------------------------------------------------
connect_message_test_() ->
    [
     {"clean_session=true, keep_alive_timer=10, client_id=hoge",
      fun () ->
              ClientId = <<"hoge">>,
              Msg = mqttm:make_connect(ClientId, [{clean_session, true},
                                                  {keep_alive_timer, 10}]),
              Expected =
                  <<
                    ?FIXED_HEADER(?MESSAGE_TYPE_CONNECT, 0, 0, 0, 18)/binary,
                    ?ENCODE_STRING(?MQTTM_PROTOCOL_NAME),
                    ?MQTTM_PROTOOCL_VERSION,
                    0:1, % username flag
                    0:1, % password flag
                    0:1, % will retain flag
                    0:2, % QoS
                    0:1, % will flag
                    1:1, % clean session flag
                    0:1, % reserved
                    10:16, % keep alive timer
                    ?ENCODE_STRING(ClientId) % client id
                  >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end},
     {"clean_session=false, keep_alive_timer=10, client_id=hoge, username=hoge, password=fuga",
      fun () ->
              ClientId = <<"hoge">>,
              Username = <<"hoge">>,
              Password = <<"fuga">>,
              Msg = mqttm:make_connect(ClientId, [{clean_session, false},
                                                  {keep_alive_timer, 10},
                                                  {username, Username},
                                                  {password, Password}]),
              Expected =
                  <<
                    ?FIXED_HEADER(?MESSAGE_TYPE_CONNECT, 0, 0, 0, 30)/binary,
                    ?ENCODE_STRING(?MQTTM_PROTOCOL_NAME),
                    ?MQTTM_PROTOOCL_VERSION,
                    1:1, % username flag
                    1:1, % password flag
                    0:1, % will retain flag
                    0:2, % Will QoS
                    0:1, % will flag
                    0:1, % clean session flag
                    0:1, % reserved
                    10:16, % keep alive timer
                    ?ENCODE_STRING(ClientId), % client id
                    ?ENCODE_STRING(Username), % username
                    ?ENCODE_STRING(Password)  % password
                  >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end},
     {"clean_session=false, keep_alive_timer=10, client_id=hoge, will{retain=false, qos=1, topic=fuga, message=killed}",
      fun () ->
              ClientId = <<"hoge">>,
              WillTopic = <<"fuga">>,
              WillMessage = <<"killed">>,
              Will = mqttm:make_will(WillTopic, WillMessage, 1, false),
              Msg = mqttm:make_connect(ClientId, [{clean_session, false},
                                                  {keep_alive_timer, 10},
                                                  {will, Will}]),
              Expected =
                  <<
                    ?FIXED_HEADER(?MESSAGE_TYPE_CONNECT, 0, 0, 0, 32)/binary,
                    ?ENCODE_STRING(?MQTTM_PROTOCOL_NAME),
                    ?MQTTM_PROTOOCL_VERSION,
                    0:1, % username flag
                    0:1, % password flag
                    0:1, % will retain flag
                    1:2, % Will QoS
                    1:1, % will flag
                    0:1, % clean session flag
                    0:1, % reserved
                    10:16, % keep alive timer
                    ?ENCODE_STRING(ClientId),   % client id
                    ?ENCODE_STRING(WillTopic),  % will topic name
                    ?ENCODE_STRING(WillMessage) % will message
                  >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end}
    ].

connack_message_test() ->
    Code = 1,
    Msg = mqttm:make_connack(Code),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_CONNACK, 0, 0, 0, 2)/binary, 0, Code>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

publish_message_test_() ->
    [
     {"QoS=0",
      fun () ->
              Topic = <<"hoge">>,
              Payload = <<"fuga">>,
              Msg = mqttm:make_publish(Topic, Payload, false),
              Expected = <<
                           ?FIXED_HEADER(?MESSAGE_TYPE_PUBLISH, 0, 0, 0, 10)/binary,
                           ?ENCODE_STRING(Topic),
                           Payload/binary
                         >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end},
     {"Qos=1",
      fun () ->
              Topic = <<"hoge">>,
              Payload = <<"fuga">>,
              QosLevel = 1,
              MessageId = 123,
              Msg = mqttm:make_publish(Topic, Payload, true, QosLevel, false, MessageId),
              Expected = <<
                           ?FIXED_HEADER(?MESSAGE_TYPE_PUBLISH, 0, QosLevel, 1, 12)/binary,
                           ?ENCODE_STRING(Topic),
                           MessageId:16,
                           Payload/binary
                         >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end},
     {"long payload",
      fun () ->
              Topic = <<"hoge">>,
              Size = 122,
              Payload = <<0:(Size * 8)>>,
              Msg = mqttm:make_publish(Topic, Payload, false),
              Expected = <<
                           ?FIXED_HEADER(?MESSAGE_TYPE_PUBLISH, 0, 0, 0, 16#80)/binary,
                           1,
                           ?ENCODE_STRING(Topic),
                           Payload/binary
                         >>,
              ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg)))
      end}
    ].

puback_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_puback(MessageId),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_PUBACK, 0, 0, 0, 2)/binary, MessageId:16>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

pubrec_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubrec(MessageId),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_PUBREC, 0, 0, 0, 2)/binary, MessageId:16>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

pubrel_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubrel(true, MessageId),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_PUBREL, 1, 1, 0, 2)/binary, MessageId:16>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

pubcomp_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubcomp(MessageId),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_PUBCOMP, 0, 0, 0, 2)/binary, MessageId:16>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

subscribe_message_test() ->
    TopicName = <<"hoge">>,
    TopicQos = 2,
    MessageId = 123,
    Msg = mqttm:make_subscribe(false, MessageId, [{TopicName, TopicQos}]),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_SUBSCRIBE, 0, 1, 0, 9)/binary,
                 MessageId:16,
                 ?ENCODE_STRING(TopicName),
                 0:6, ?ENCODE_QOS(TopicQos)>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

suback_message_test() ->
    QosList = [0, 1, 2],
    MessageId = 124,
    Msg = mqttm:make_suback(MessageId, QosList),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_SUBACK, 0, 0, 0, 5)/binary,
                 MessageId:16,
                 (list_to_binary(QosList))/binary>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

unsubscribe_message_test() ->
    TopicList = [<<"hoge">>],
    MessageId = 124,
    Msg = mqttm:make_unsubscribe(true, MessageId, TopicList),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_UNSUBSCRIBE, 1, 1, 0, 8)/binary,
                 MessageId:16,
                 (list_to_binary([<<?ENCODE_STRING(X)>> || X <- TopicList]))/binary>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

unsuback_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_unsuback(MessageId),
    Expected = <<?FIXED_HEADER(?MESSAGE_TYPE_UNSUBACK, 0, 0, 0, 2)/binary, MessageId:16>>,
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).
    
pingreq_message_test() ->
    Msg = mqttm:make_pingreq(),
    Expected = ?FIXED_HEADER(?MESSAGE_TYPE_PINGREQ, 0, 0, 0, 0),
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

pingresp_message_test() ->
    Msg = mqttm:make_pingresp(),
    Expected = ?FIXED_HEADER(?MESSAGE_TYPE_PINGRESP, 0, 0, 0, 0),
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).

disconnect_message_test() ->
    Msg = mqttm:make_disconnect(),
    Expected = ?FIXED_HEADER(?MESSAGE_TYPE_DISCONNECT, 0, 0, 0, 0),
    ?assertEqual(Expected, iolist_to_binary(mqttm_encode:encode(Msg))).
