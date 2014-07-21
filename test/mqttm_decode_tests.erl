%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
-module(mqttm_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("mqttm.hrl").
-include("mqttm_internal.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(encode(Message), iolist_to_binary(mqttm_encode:encode(Message))).
-define(assertDecode(Message), ?assertEqual({[Message], <<>>}, mqttm_decode:decode(?encode(Message)))).

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
              ?assertDecode(Msg)
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
              ?assertDecode(Msg)
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
              ?assertDecode(Msg)
      end}
    ].

connack_message_test() ->
    Code = 1,
    Msg = mqttm:make_connack(Code),
    ?assertDecode(Msg).

publish_message_test_() ->
    [
     {"QoS=0",
      fun () ->
              Topic = <<"hoge">>,
              Payload = <<"fuga">>,
              Msg = mqttm:make_publish(Topic, Payload, false),
              ?assertDecode(Msg)
      end},
     {"Qos=1",
      fun () ->
              Topic = <<"hoge">>,
              Payload = <<"fuga">>,
              QosLevel = 1,
              MessageId = 123,
              Msg = mqttm:make_publish(Topic, Payload, true, QosLevel, false, MessageId),
              ?assertDecode(Msg)
      end},
     {"long payload",
      fun () ->
              Topic = <<"hoge">>,
              Size = 122,
              Payload = <<0:(Size * 8)>>,
              Msg = mqttm:make_publish(Topic, Payload, false),
              ?assertDecode(Msg)
      end}
    ].

puback_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_puback(MessageId),
    ?assertDecode(Msg).

pubrec_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubrec(MessageId),
    ?assertDecode(Msg).

pubrel_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubrel(true, MessageId),
    ?assertDecode(Msg).

pubcomp_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_pubcomp(MessageId),
    ?assertDecode(Msg).

subscribe_message_test() ->
    TopicName = <<"hoge">>,
    TopicQos = 2,
    MessageId = 123,
    Msg = mqttm:make_subscribe(false, MessageId, [{TopicName, TopicQos}]),
    ?assertDecode(Msg).

suback_message_test() ->
    QosList = [0, 1, 2],
    MessageId = 124,
    Msg = mqttm:make_suback(MessageId, QosList),
    ?assertDecode(Msg).

unsubscribe_message_test() ->
    TopicList = [<<"hoge">>],
    MessageId = 124,
    Msg = mqttm:make_unsubscribe(true, MessageId, TopicList),
    ?assertDecode(Msg).

unsuback_message_test() ->
    MessageId = 123,
    Msg = mqttm:make_unsuback(MessageId),
    ?assertDecode(Msg).
    
pingreq_message_test() ->
    Msg = mqttm:make_pingreq(),
    ?assertDecode(Msg).

pingresp_message_test() ->
    Msg = mqttm:make_pingresp(),
    ?assertDecode(Msg).

disconnect_message_test() ->
    Msg = mqttm:make_disconnect(),
    ?assertDecode(Msg).
