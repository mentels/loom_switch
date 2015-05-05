-module(ls_ofsh).

-export([init/7, handle_message/2, terminate/1]).

-record(state, {datapath_id}).
-type state() :: #state{}.

-include_lib("ofs_handler/include/ofs_handler.hrl").

init(_Mode, _Ip, DatapathId, _Features, _Version, _Connection, _Options) ->
    ok = ls_logic:init_main_connection(DatapathId),
    {ok, #state{datapath_id = DatapathId}}.

handle_message({packet_in, Xid, Body}, #state{datapath_id = DpId}) ->
    ls_logic:handle_packet_in(DpId, {Xid, Body});
handle_message(_Msg, _State) ->
    ok.

terminate(#state{datapath_id = DpId}) ->
    ok = ls_logic:terminate_main_connection(DpId).
