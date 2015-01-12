-module(ls_logic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DPID, "00:00:00:00:00:00:00:01").
-define(RAW_PACKET, <<255,255,255,255,255,255,50,159,179, 220,238,226,
                      8,6,0,1,8,0,6,4,0,1,50, 159,179,220,238,226,10,
                      0,0,1,0,0,0,0,0,0,10,0,0,2>>).

all() ->
    [should_update_fwd_table].

init_per_suite(Config) ->
    mock_ofs_handler(?DPID),
    mock_bogus(),
    [{dpid, ?DPID}, {raw_packet, ?RAW_PACKET} | Config].

end_per_suite(Config) ->
    ok = ls_logic:terminate_main_connection(?config(dpid, Config)),
    ok = meck:unload(bogus),
    ok = meck:unload(ofs_handler).

should_update_fwd_table(Config) ->
    %% GIVEN
    DpId = ?config(dpid, Config),
    RawPacket = ?config(raw_packet, Config),
    SrcMac = raw_packet_value(src_mac, RawPacket),
    {ok, _Pid} = ls_logic:start_link(),
    ok = ls_logic:init_main_connection(DpId),
    PacketIn = packet_in(RawPacket, InPort = <<0,0,0,1>>),

    %% WHEN
    ok = ls_logic:handle_packet_in(DpId, PacketIn),

    %% THEN
    {ok, FwdTable} = ls_logic:get_forwarding_table(DpId),
    ?assertEqual(InPort, maps:get(SrcMac, FwdTable)).
    
mock_ofs_handler(DpId) ->
    ok = meck:expect(ofs_handler, subscribe, [{[DpId, ls_ofsh, packet_in], ok}]),
    ok = meck:expect(ofs_handler, unsubscribe, [{[DpId, ls_ofsh, packet_in], ok}]).

mock_bogus() ->
    ok = meck:new(bogus, [non_strict, no_link]),
    ok = meck:expect(bogus, send, fun(_) -> ok end).

packet_in(RawPacket, InPort) ->
    [{buffer_id,261},
     {reason,no_match},
     {table_id,0},
     {cookie,<<0,0,0,0,0,0,0,0>>},
     {match,[{in_port,InPort}]},
     {data, RawPacket}].

raw_packet_value(src_mac, <<_DstMac:6/bytes, SrcMac:6/bytes, _/binary>>) ->
    SrcMac.
    

