-module(ls_ofsh2).

-export([init/7, handle_message/2, terminate/1]).

-record(state, {logic_pid :: pid()}).

-include_lib("ofs_handler/include/ofs_handler.hrl").

init(_Mode, _Ip, DatapathId, _Features, _Version, _Connection, _Options) ->
    {ok, Pid} = ls_logic2:init_main_connection(DatapathId),
    {ok, #state{logic_pid = Pid}}.

handle_message({packet_in, Xid, Body}, #state{logic_pid = Pid}) ->
    ls_logic2:handle_packet_in(Pid, {Xid, Body});
handle_message(_Msg, _State) ->
    ok.

terminate(#state{logic_pid = Pid}) ->
    ok = ls_logic2:terminate_main_connection(Pid).
