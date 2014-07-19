%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc MQTT message
-module(mqttmsg).

-include("mqttmsg.hrl").

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([encode/1]).
-export([decode/1]).

-export_type([message/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type message() :: term().

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec encode(message()) -> iodata().
encode(Message) ->
    error(not_implemented, [Message]).

-spec decode(binary()) -> {message(), RemainingBytes::binary()}.
decode(Bytes) ->
    error(not_implemented, [Bytes]).