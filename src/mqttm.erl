%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(mqttm).

-include("mqttm.hrl").
-include("mqttm_internal.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([encode/1]).
-export([decode/1]).

-export([make_connect/2]).
-export([make_connack/1]).
-export([make_publish/3, make_publish/6]).
-export([make_puback/1]).
-export([make_pubrec/1]).
-export([make_pubrel/2]).
-export([make_pubcomp/1]).
-export([make_subscribe/3]).
-export([make_suback/2]).
-export([make_unsubscribe/3]).
-export([make_unsuback/1]).
-export([make_pingreq/0]).
-export([make_pingresp/0]).
-export([make_disconnect/0]).

-export([make_will/4]).

-export_type([message/0]).
-export_type([connect_message/0]).
-export_type([connack_message/0]).
-export_type([publish_message/0]).
-export_type([puback_message/0]).
-export_type([pubrec_message/0]).
-export_type([pubrel_message/0]).
-export_type([pubcomp_message/0]).
-export_type([subscribe_message/0]).
-export_type([suback_message/0]).
-export_type([unsubscribe_message/0]).
-export_type([unsuback_message/0]).
-export_type([pingreq_message/0]).
-export_type([pingresp_message/0]).
-export_type([disconnect_message/0]).

-export_type([qos_level/0, qos_level_0/0, qos_level_1/0, qos_level_2/0]).
-export_type([flag/0, dup_flag/0]).
-export_type([non_neg_seconds/0]).
-export_type([topic_name/0]).
-export_type([client_id/0]).
-export_type([message_id/0]).
-export_type([will/0]).
-export_type([connect_return_code/0]).
-export_type([connect_option/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type message() :: connect_message()
                 | connack_message()
                 | publish_message()
                 | puback_message()
                 | pubrec_message()
                 | pubrel_message()
                 | pubcomp_message()
                 | subscribe_message()
                 | suback_message()
                 | unsubscribe_message()
                 | unsuback_message()
                 | pingreq_message()
                 | pingresp_message()
                 | disconnect_message()
                 | unsuback_message().

-type connect_message()     :: #mqttm_connect{}.
-type connack_message()     :: #mqttm_connack{}.
-type publish_message()     :: #mqttm_publish{}.
-type puback_message()      :: #mqttm_puback{}.
-type pubrec_message()      :: #mqttm_pubrec{}.
-type pubrel_message()      :: #mqttm_pubrel{}.
-type pubcomp_message()     :: #mqttm_pubcomp{}.
-type subscribe_message()   :: #mqttm_subscribe{}.
-type suback_message()      :: #mqttm_suback{}.
-type unsubscribe_message() :: #mqttm_unsubscribe{}.
-type unsuback_message()    :: #mqttm_unsuback{}.
-type pingreq_message()     :: #mqttm_pingreq{}.
-type pingresp_message()    :: #mqttm_pingresp{}.
-type disconnect_message()  :: #mqttm_disconnect{}.

-type qos_level() :: qos_level_0()
                   | qos_level_1()
                   | qos_level_2().
-type qos_level_0() :: 0.
-type qos_level_1() :: 1.
-type qos_level_2() :: 2.

-type flag()                :: boolean().
-type dup_flag()            :: flag().
-type non_neg_seconds()     :: non_neg_integer().
-type topic_name()          :: binary().
-type client_id()           :: binary().
-type message_id()          :: 0..16#FFFF.
-type connect_return_code() :: byte().
-type will()                :: #mqttm_will{}.

-type connect_option() :: {clean_session, flag()} % default: true
                        | {keep_alive_timer, non_neg_seconds()} % defualt: 120
                        | {username, binary()}
                        | {password, binary()}
                        | {will, will()}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec encode(message()) -> iodata().
encode(Message) ->
    mqttm_encode:encode(Message).
    
-spec decode(binary()) -> {[message()], RemainingBytes::binary()}.
decode(Bytes) ->
    mqttm_decode:decode(Bytes).

-spec make_connect(client_id(), [connect_option()]) -> connect_message().
make_connect(ClientId, Options) when ?IS_STRING(ClientId), is_list(Options) ->
    CleanSessionFlag = proplists:get_value(clean_session, Options, true),
    KeepAliveTimer = proplists:get_value(keep_alive_timer, Options, ?DEFAULT_KEEP_ALIVE_TIMER),
    UserName = proplists:get_value(username, Options, undefined),
    Password = proplists:get_value(password, Options, undefined),
    Will = proplists:get_value(will, Options, undefined),

    IsValid =
        is_boolean(CleanSessionFlag) andalso
        is_integer(KeepAliveTimer) andalso KeepAliveTimer >= 0 andalso
        (UserName =:= undefined orelse ?IS_STRING(UserName)) andalso
        (Password =:= undefined orelse ?IS_STRING(Password)) andalso
        (Will =:= undefined orelse is_record(Will, mqttm_will)),
    case IsValid of
        false -> error(badarg, [ClientId, Options]);
        true  ->
            #mqttm_connect{
               clean_session_flag = CleanSessionFlag,
               keep_alive_timer   = KeepAliveTimer,
               client_id          = ClientId,
               will               = Will,
               username           = UserName,
               password           = Password
              }
    end;
make_connect(ClientId, Options) ->
    error(badarg, [ClientId, Options]).

-spec make_connack(connect_return_code()) -> connack_message().
make_connack(Code) when is_integer(Code), 0 =< Code, Code =< 16#FF ->
    #mqttm_connack{return_code = Code};
make_connack(Code) -> error(badarg, [Code]).

-spec make_publish(topic_name(), binary(), flag()) -> publish_message().
make_publish(Topic, Payload, RetainFlag) when ?IS_STRING(Topic), is_binary(Payload), is_boolean(RetainFlag) ->
    #mqttm_publish{
       dup_flag    = false,
       qos_level   = 0,
       retain_flag = RetainFlag,
       topic_name  = Topic,
       payload     = Payload
      };
make_publish(Topic, Payload, RetainFlag) ->
    error(badarg, [Topic, Payload, RetainFlag]).

-spec make_publish(topic_name(), binary(), flag(), qos_level_1()|qos_level_2(), dup_flag(), message_id()) -> publish_message().
make_publish(Topic, Payload, RetainFlag, QosLevel, DupFlag, MessageId) when
      ?IS_STRING(Topic), is_binary(Payload), is_boolean(RetainFlag), is_boolean(DupFlag),
      (QosLevel =:= 1 orelse QosLevel =:= 2), ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_publish{
       dup_flag    = DupFlag,
       qos_level   = QosLevel,
       retain_flag = RetainFlag,
       topic_name  = Topic,
       message_id  = MessageId,
       payload     = Payload
      };
make_publish(Topic, Payload, RetainFlag, QosLevel, DupFlag, MessageId) ->
    error(badarg, [Topic, Payload, RetainFlag, QosLevel, DupFlag, MessageId]).

-spec make_puback(message_id()) -> puback_message().
make_puback(MessageId) when ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_puback{message_id = MessageId};
make_puback(MessageId) -> error(badarg, [MessageId]).

-spec make_pubrec(message_id()) -> pubrec_message().
make_pubrec(MessageId) when ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_pubrec{message_id = MessageId};
make_pubrec(MessageId) -> error(badarg, [MessageId]).

-spec make_pubrel(dup_flag(), message_id()) -> pubrel_message().
make_pubrel(DupFlag, MessageId) when is_boolean(DupFlag), ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_pubrel{dup_flag = DupFlag, message_id = MessageId};
make_pubrel(DupFlag, MessageId) -> error(badarg, [DupFlag, MessageId]).

-spec make_pubcomp(message_id()) -> pubcomp_message().
make_pubcomp(MessageId) when ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_pubcomp{message_id = MessageId};
make_pubcomp(MessageId) -> error(badarg, [MessageId]).

-spec make_subscribe(dup_flag(), message_id(), [{topic_name(), qos_level()}]) -> subscribe_message().
make_subscribe(DupFlag, MessageId, TopicList) when is_boolean(DupFlag), ?IS_MESSAGE_ID(MessageId), is_list(TopicList) ->
    case lists:all(fun ({Topic, Qos}) -> ?IS_STRING(Topic) andalso ?IS_QOS_LEVEL(Qos);
                       (_)            -> false
                   end, TopicList) of
        false -> error(badarg, [DupFlag, MessageId, TopicList]);
        true  ->
            #mqttm_subscribe{
               dup_flag   = DupFlag,
               message_id = MessageId,
               payload    = TopicList
              }
    end;
make_subscribe(DupFlag, MessageId, TopicList) ->
    error(badarg, [DupFlag, MessageId, TopicList]).

-spec make_suback(message_id(), [qos_level()]) -> suback_message().
make_suback(MessageId, QosLevelList) when ?IS_MESSAGE_ID(MessageId), is_list(QosLevelList) ->
    case lists:all(fun (Qos) -> ?IS_QOS_LEVEL(Qos) end, QosLevelList) of
        false -> error(badarg, [MessageId, QosLevelList]);
        true  ->
            #mqttm_suback{
               message_id = MessageId,
               payload    = QosLevelList
              }
    end;
make_suback(MessageId, QosLevelList) ->
    error(badarg, [MessageId, QosLevelList]).

-spec make_unsubscribe(dup_flag(), message_id(), [topic_name()]) -> unsubscribe_message().
make_unsubscribe(DupFlag, MessageId, TopicList) when is_boolean(DupFlag), ?IS_MESSAGE_ID(MessageId), is_list(TopicList) ->
    case lists:all(fun (Topic) -> ?IS_STRING(Topic) end, TopicList) of
        false -> error(badarg, [DupFlag, MessageId, TopicList]);
        true  ->
            #mqttm_unsubscribe{
               dup_flag   = DupFlag,
               message_id = MessageId,
               payload    = TopicList
              }
    end;
make_unsubscribe(DupFlag, MessageId, TopicList) ->
    error(badarg, [DupFlag, MessageId, TopicList]).

-spec make_unsuback(message_id()) -> unsuback_message().
make_unsuback(MessageId) when ?IS_MESSAGE_ID(MessageId) ->
    #mqttm_unsuback{message_id = MessageId};
make_unsuback(MessageId) ->
    error(badarg, [MessageId]).
    
-spec make_pingreq() -> pingreq_message().
make_pingreq() -> #mqttm_pingreq{}.

-spec make_pingresp() -> pingresp_message().
make_pingresp() -> #mqttm_pingresp{}.

-spec make_disconnect() -> disconnect_message().
make_disconnect() -> #mqttm_disconnect{}.

-spec make_will(topic_name(), binary(), qos_level(), flag()) -> will().
make_will(TopicName, Message, QosLevel, RetainFlag) when
      ?IS_STRING(TopicName), ?IS_STRING(Message), ?IS_QOS_LEVEL(QosLevel), is_boolean(RetainFlag) ->
    #mqttm_will{
       qos_level   = QosLevel,
       retain_flag = RetainFlag,
       topic_name  = TopicName,
       message     = Message
      };
make_will(TopicName, Message, QosLevel, RetainFlag) ->
    error(badarg, [TopicName, Message, QosLevel, RetainFlag]).
