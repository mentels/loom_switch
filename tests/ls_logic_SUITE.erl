-module(ls_logic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DPID, "00:00:00:00:00:00:00:01").
-define(PACKET, <<255,255,255,255,255,255,50,159,179, 220,238,226,
                      8,6,0,1,8,0,6,4,0,1,50, 159,179,220,238,226,10,
                      0,0,1,0,0,0,0,0,0,10,0,0,2>>).

all() ->
    [should_update_fwd_table].

init_per_suite(Config) ->
    mock_ofs_handler(DpId = ?DPID),
    mock_bogus(),
    init_ls_logic(DpId),
    [{dpid, DpId}, {packet, ?PACKET} | Config].

end_per_suite(Config) ->
    stop_ls_logic(?config(dpid, Config)),
    ok = meck:unload(bogus),
    ok = meck:unload(ofs_handler).

should_update_fwd_table(Config) ->
    %% GIVEN
    SrcMac = extract_from_packet(src_mac, Packet = ?config(packet, Config)),
    PacketIn = packet_in(Packet, InPort = <<0,0,0,1>>),

    %% WHEN
    ok = ls_logic:handle_packet_in(DpId = ?config(dpid, Config), PacketIn),

    %% THEN
    {ok, FwdTable} = ls_logic:get_forwarding_table(DpId),
    ?assertEqual(InPort, maps:get(SrcMac, FwdTable)).

init_ls_logic(DpId) ->
    {ok, _Pid} = ls_logic:start(),
    ok = ls_logic:init_main_connection(DpId).

stop_ls_logic(DpId) ->
    ok = ls_logic:terminate_main_connection(DpId),
    ok = ls_logic:stop().

mock_ofs_handler(DpId) ->
    ok = meck:expect(ofs_handler, subscribe,
                     [{Args = [DpId, ls_ofsh, packet_in], Ret = ok}]),
    ok = meck:expect(ofs_handler, unsubscribe, [{Args, Ret}]).

mock_bogus() ->
    ok = meck:new(bogus, [_AllowNonExistentModule = non_strict, no_link]),
    ok = meck:expect(bogus, send, fun(_) -> ok end).

packet_in(Packet, InPort) ->
    [{buffer_id,261},
     {reason,no_match},
     {table_id,0},
     {cookie,<<0,0,0,0,0,0,0,0>>},
     {match,[{in_port,InPort}]},
     {data, Packet}].

extract_from_packet(src_mac, <<_DstMac:6/bytes, SrcMac:6/bytes, _/binary>>) ->
    SrcMac.
