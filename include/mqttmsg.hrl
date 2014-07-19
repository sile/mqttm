%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc external header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(MQTTMSG_PROTOCOL_NAME, <<"MQIsdp">>).
-define(MQTTMSG_PROTOOCL_VERSION, 3).

%%------------------------------------------------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------------------------------------------------
-record(mqttmsg_connect,
        {
          protocol_name    = ?MQTTMSG_PROTOCOL_NAME    :: binary(),
          protocol_version = ?MQTTMSG_PROTOOCL_VERSION :: byte(),
          clean_session_flag :: mqttmsg:flag(),
          keep_alive_timer   :: mqttmsg:non_neg_seconds(),
          client_id          :: mqttmsg:client_id(),
          will               :: undefined | mqttmsg:will(),
          username           :: undefined | binary(),
          password           :: undefined | binary()
        }).

-record(mqttmsg_connack,
        {
          return_code :: mqttmsg:connect_return_code()
        }).

-record(mqttmsg_publish,
        {
          retain_flag       :: mqttmsg:flag(),
          transmission_info :: mqttmsg:transmission_info(),
          topic_name        :: mqttmsg:topic_name(),
          payload           :: binary()
        }).

-record(mqttmsg_puback,
        {
          message_id :: mqttmsg:message_id()
        }).

-record(mqttmsg_pubrec,
        {
          message_id :: mqttmsg:message_id()
        }).

-record(mqttmsg_pubrel,
        {
          transmission_info :: mqttmsg:transmission_info()
        }).

-record(mqttmsg_pubcomp,
        {
          message_id :: mqttmsg:message_id()
        }).

-record(mqttmsg_subscribe,
        {
          transmission_info :: mqttmsg:transmission_info(),
          payload           :: [{mqttmsg:topic_name(), mqttmsg:qos_level()}]
        }).

-record(mqttmsg_suback,
        {
          message_id :: mqttmsg:message_id(),
          payload    :: [mqttmsg:qos_level()]
        }).

-record(mqttmsg_unsubscribe,
        {
          transmission_info :: mqttmsg:transmission_info(),
          payload           :: [mqttmsg:topic_name()]
        }).

-record(mqttmsg_unsuback,
        {
          message_id :: mqttmsg:message_id()
        }).

-record(mqttmsg_pingreq,
        {
        }).

-record(mqttmsg_pingresp,
        {
        }).

-record(mqttmsg_disconnect,
        {
        }).

-record(mqttmsg_unknown,
        {
          message_type :: byte(),
          dup_flag     :: mqttmsg:flag(),
          qos_level    :: mqttmsg:qos_level(),
          retain_flag  :: mqttmsg:flag(),
          payload      :: binary()
        }).

-record(mqttmsg_will,
        {
          qos_level   :: mqttmsg:qos_level(),
          retain_flag :: mqttmsg:flag(),
          topic_name  :: mqttmsg:topic_name(),
          message     :: binary()
        }).
