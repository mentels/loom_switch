-module(ls_logic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DPID, "00:00:00:00:00:00:00:01").
-define(BUFFER_ID, 15).
-define(MAC(LastByte), <<50,159,179,220,238, LastByte>>).
-define(PACKET(Dst, Src), <<Dst/binary, Src/binary, 8,6,0,1,8,0,6,4,0,1,50,
                            159,179,220,238,226,10, 0,0,1,0,0,0,0,0,0,10,0,0,2>>).

all() ->
    [should_update_fwd_table,
     should_flood_if_dst_port_unknown,
     should_install_flow_and_forward_if_dst_port_known].

init_per_suite(Config) ->
    mock_ofs_handler(DpId = ?DPID),
    init_ls_logic(DpId),
    Config.

end_per_suite(_Config) ->
    stop_ls_logic(?DPID),
    ok = meck:unload(ofs_handler).

should_update_fwd_table(_Config) ->
    %% GIVEN
    Packet = ?PACKET(?MAC(100), (SrcMac = ?MAC(200))),
    PacketIn = packet_in(Packet, InPort = 1),

    %% WHEN
    ok = ls_logic:handle_packet_in(?DPID, PacketIn),

    %% THEN
    {ok, FwdTable} = ls_logic:get_forwarding_table(?DPID),
    ?assertEqual(InPort, maps:get(SrcMac, FwdTable)).

should_flood_if_dst_port_unknown(_Config) ->
    %% GIVEN
    PacketIn = packet_in(?PACKET(?MAC(100), ?MAC(150)), InPort = 1),
    
    %% GIVEN
    ls_logic:handle_packet_in(?DPID, PacketIn),

    %% THEN
    Actions = [{output, flood, no_buffer}],
    ExpectedPacketOut = of_msg_lib:send_packet(4, ?BUFFER_ID, InPort, Actions),
    meck:wait(1, ofs_handler, send, [?DPID, ExpectedPacketOut], 2000).

should_install_flow_and_forward_if_dst_port_known(_Config) ->
    %% GIVEN
    learn_mac_to_port(LearnedMac = ?MAC(100), LearnedPort = 1),
        
    %% WHEN
    PacketIn = packet_in(?PACKET(LearnedMac, ?MAC(150)), InPort = 2),
    ls_logic:handle_packet_in(?DPID, PacketIn),

    %% THEN
    Matches = [{in_port, InPort}, {eth_dst, LearnedMac}],
    Instructions = [{apply_actions, [{output, LearnedPort, no_buffer}]}],
    Opts = [{table_id,0}, {priority, 100},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    ExpectedFlowMod = of_msg_lib:flow_add(4, Matches, Instructions, Opts),
    meck:wait(1, ofs_handler, send, [?DPID, ExpectedFlowMod], 2000),

    Actions = [{output, LearnedPort, no_buffer}],
    ExpectedPacketOut = of_msg_lib:send_packet(4, ?BUFFER_ID, InPort, Actions),
    meck:wait(1, ofs_handler, send, [?DPID, ExpectedPacketOut], 2000).


learn_mac_to_port(Mac, Port) ->
    PacketIn = packet_in(?PACKET(?MAC(200), Mac), Port),
    ok = ls_logic:handle_packet_in(?DPID, PacketIn).

init_ls_logic(DpId) ->
    {ok, _Pid} = ls_logic:start(),
    ok = ls_logic:init_main_connection(DpId).

stop_ls_logic(DpId) ->
    ok = ls_logic:terminate_main_connection(DpId),
    ok = ls_logic:stop().

mock_ofs_handler(DpId) ->
    ok = meck:expect(ofs_handler, subscribe,
                     [{Args = [DpId, ls_ofsh, packet_in], Ret = ok}]),
    ok = meck:expect(ofs_handler, unsubscribe, [{Args, Ret}]),
    ok = meck:expect(ofs_handler, send, 2, ok).

packet_in(Packet, InPort) ->
    [{buffer_id,?BUFFER_ID},
     {reason,no_match},
     {table_id,0},
     {cookie,<<0,0,0,0,0,0,0,0>>},
     {match,[{in_port,<<InPort:32>>}]},
     {data, Packet}].
