%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc internal header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(MESSAGE_TYPE_CONNECT, 1).
-define(MESSAGE_TYPE_CONNACK, 2).
-define(MESSAGE_TYPE_PUBLISH, 3).
-define(MESSAGE_TYPE_PUBACK, 4).
-define(MESSAGE_TYPE_PUBREC, 5).
-define(MESSAGE_TYPE_PUBREL, 6).
-define(MESSAGE_TYPE_PUBCOMP, 7).
-define(MESSAGE_TYPE_SUBSCRIBE, 8).
-define(MESSAGE_TYPE_SUBACK, 9).
-define(MESSAGE_TYPE_UNSUBSCRIBE, 10).
-define(MESSAGE_TYPE_UNSUBACK, 11).
-define(MESSAGE_TYPE_PINGREQ, 12).
-define(MESSAGE_TYPE_PINGRESP, 13).
-define(MESSAGE_TYPE_DISCONNECT, 14).

-define(IS_QOS_LEVEL(X), (is_integer(X) andalso 0 =< X andalso X =< 2)).
-define(IS_STRING(X), (is_binary(X) andalso byte_size(X) =< 16#FFFF)).
-define(IS_MESSAGE_ID(X), (is_integer(MessageId) andalso 0 =< MessageId andalso MessageId =< 16#FFFF)).

-define(BOOL_TO_INT(X), (case X of true -> 1; false -> 0 end)).

-define(ENCODE_STRING(S), (byte_size(S)):16, (S)/binary).
-define(ENCODE_STRING_IF_DEFINED(X, S), (case X of
                                             undefined -> <<>>;
                                             _         -> <<?ENCODE_STRING(S)>>
                                         end)/binary).
-define(ENCODE_STRING_IF_DEFINED(S), ?ENCODE_STRING_IF_DEFINED(S, S)).

-define(ENCODE_FLAG(B), ?BOOL_TO_INT(B):1).
-define(ENCODE_QOS(X), (X):2).

-define(ENCODE_HEADER_FIRST(Type), (Type bsl 4)).
-define(ENCODE_HEADER_FIRST(Type, DupFlag, QosLevel),
        ((Type bsl 4) bor (?BOOL_TO_INT(DupFlag) bsl 3) bor (QosLevel bsl 1))).
-define(ENCODE_HEADER_FIRST(Type, DupFlag, QosLevel, RetainFlag),
        ((Type bsl 4) bor (?BOOL_TO_INT(DupFlag) bsl 3) bor (QosLevel bsl 1) bor (?BOOL_TO_INT(RetainFlag)))).
