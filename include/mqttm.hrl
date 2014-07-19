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
-record(mqttm_connect,
        {
          protocol_name    = ?MQTTMSG_PROTOCOL_NAME    :: binary(),
          protocol_version = ?MQTTMSG_PROTOOCL_VERSION :: byte(),
          clean_session_flag :: mqttm:flag(),
          keep_alive_timer   :: mqttm:non_neg_seconds(),
          client_id          :: mqttm:client_id(),
          will               :: undefined | mqttm:will(),
          username           :: undefined | binary(),
          password           :: undefined | binary()
        }).

-record(mqttm_connack,
        {
          return_code :: mqttm:connect_return_code()
        }).

-record(mqttm_publish,
        {
          retain_flag       :: mqttm:flag(),
          transmission_info :: mqttm:transmission_info(),
          topic_name        :: mqttm:topic_name(),
          payload           :: binary()
        }).

-record(mqttm_puback,
        {
          message_id :: mqttm:message_id()
        }).

-record(mqttm_pubrec,
        {
          message_id :: mqttm:message_id()
        }).

-record(mqttm_pubrel,
        {
          transmission_info :: mqttm:transmission_info()
        }).

-record(mqttm_pubcomp,
        {
          message_id :: mqttm:message_id()
        }).

-record(mqttm_subscribe,
        {
          transmission_info :: mqttm:transmission_info(),
          payload           :: [{mqttm:topic_name(), mqttm:qos_level()}]
        }).

-record(mqttm_suback,
        {
          message_id :: mqttm:message_id(),
          payload    :: [mqttm:qos_level()]
        }).

-record(mqttm_unsubscribe,
        {
          transmission_info :: mqttm:transmission_info(),
          payload           :: [mqttm:topic_name()]
        }).

-record(mqttm_unsuback,
        {
          message_id :: mqttm:message_id()
        }).

-record(mqttm_pingreq,
        {
        }).

-record(mqttm_pingresp,
        {
        }).

-record(mqttm_disconnect,
        {
        }).

-record(mqttm_unknown,
        {
          message_type :: byte(),
          dup_flag     :: mqttm:flag(),
          qos_level    :: mqttm:qos_level(),
          retain_flag  :: mqttm:flag(),
          payload      :: binary()
        }).

-record(mqttm_will,
        {
          qos_level   :: mqttm:qos_level(),
          retain_flag :: mqttm:flag(),
          topic_name  :: mqttm:topic_name(),
          message     :: binary()
        }).
