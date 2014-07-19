%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc MQTT message
-module(mqttmsg).

-include("mqttmsg.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([encode/1, try_encode/1]).
-export([decode/1, try_decode/1]).

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
-export_type([unknown_message/0]).

-export_type([qos_level/0, qos_level_0/0, qos_level_1/0, qos_level_2/0]).
-export_type([transmission_info/0]).
-export_type([flag/0, dup_flag/0]).
-export_type([non_neg_seconds/0]).
-export_type([topic_name/0]).
-export_type([client_id/0]).
-export_type([message_id/0]).
-export_type([will/0]).
-export_type([connect_return_code/0]).

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

-type connect_message()     :: #mqttmsg_connect{}.
-type connack_message()     :: #mqttmsg_connack{}.
-type publish_message()     :: #mqttmsg_publish{}.
-type puback_message()      :: #mqttmsg_puback{}.
-type pubrec_message()      :: #mqttmsg_pubrec{}.
-type pubrel_message()      :: #mqttmsg_pubrel{}.
-type pubcomp_message()     :: #mqttmsg_pubcomp{}.
-type subscribe_message()   :: #mqttmsg_subscribe{}.
-type suback_message()      :: #mqttmsg_suback{}.
-type unsubscribe_message() :: #mqttmsg_unsubscribe{}.
-type unsuback_message()    :: #mqttmsg_unsuback{}.
-type pingreq_message()     :: #mqttmsg_pingreq{}.
-type pingresp_message()    :: #mqttmsg_pingresp{}.
-type disconnect_message()  :: #mqttmsg_disconnect{}.
-type unknown_message()     :: #mqttmsg_unknown{}.

-type qos_level() :: qos_level_0()
                   | qos_level_1()
                   | qos_level_2().
-type qos_level_0() :: 0.
-type qos_level_1() :: 1.
-type qos_level_2() :: 2.

-type transmission_info() :: qos_level_0()
                           | {qos_level_1(), dup_flag(), message_id()}
                           | {qos_level_2(), dup_flag(), message_id()}.

-type flag()                :: 0 | 1.
-type dup_flag()            :: flag().
-type non_neg_seconds()     :: non_neg_integer().
-type topic_name()          :: binary().
-type client_id()           :: binary().
-type message_id()          :: 0..16#FFFF.
-type connect_return_code() :: byte().
-type will()                :: #mqttmsg_will{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec encode(message()) -> iodata().
encode(Message) ->
    try
        {ok, IoData} = try_encode(Message),
        IoData
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ->
            erlang:raise(error, Reason, [StackItem | erlang:get_stacktrace()])
    end.

-spec try_encode(message()) -> {ok, iodata()} | {error, {Reason::term(), [erlang:stack_item()]}}.
try_encode(Message) ->
    error(not_implemented, [Message]).

-spec decode(binary()) -> {message(), RemainingBytes::binary()}.
decode(Bytes) ->
    try
        {ok, Message, RemainingBytes} = try_decode(Bytes),
        {Message, RemainingBytes}
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ->
            erlang:raise(error, Reason, [StackItem | erlang:get_stacktrace()])
    end.

-spec try_decode(binary()) -> {ok, message(), RemainingBytes::binary()}  | {error, {Reason::term(), [erlang:stack_item()]}}.
try_decode(Bytes) ->
    error(not_implemented, [Bytes]).
