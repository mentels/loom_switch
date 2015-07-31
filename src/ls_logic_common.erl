-module(ls_logic_common).

-export([handle_packet_in/6,
         remove_forwarding_entry/2]).

-include_lib("of_protocol/include/of_protocol.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

handle_packet_in(DatapathId, Xid, PacketIn, FwdTable0, T1, Opts) ->
    FwdTable1  = learn_src_mac_to_port(DatapathId, PacketIn, FwdTable0),
    OutPort = case get_port_for_dst_mac(PacketIn, FwdTable0) of
                  undefined ->
                      flood;
                  PortNo ->
                      install_flow_to_dst_mac(PacketIn, PortNo, DatapathId, Opts),
                      lager:debug([{ls, x}], "[~p][flow] Sent flow mod", [DatapathId]),
                      PortNo
              end,
    send_packet_out(DatapathId, Xid, PacketIn, OutPort),
    update_handle_packet_in_metric(T1),
    lager:debug([{ls, x}], "[~p][pkt_out] Sent packet out through port: ~p~n", [DatapathId,
                                                                                OutPort]),
    FwdTable1.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

learn_src_mac_to_port(DatapathId, PacketIn, FwdTable0) ->
    [InPort, SrcMac] = packet_in_extract([in_port, src_mac], PacketIn),
    case maps:get(SrcMac, FwdTable0, undefined) of
        InPort ->
            FwdTable0;
        _ ->
            FwdTable1 = maps:put(SrcMac, InPort, FwdTable0),
            schedule_removing_entry(DatapathId, SrcMac),
            lager:debug([{ls, x}], "[~p][pkt_in] Added entry to fwd table: ~p -> ~p ~n",
                        [DatapathId, format_mac(SrcMac), InPort]),
            FwdTable1
    end.


get_port_for_dst_mac(PacketIn, FwdTable) ->
    DstMac = packet_in_extract(dst_mac, PacketIn),
    case maps:find(DstMac, FwdTable) of
        error ->
            undefined;
        {ok, Port} ->
            Port
    end.

install_flow_to_dst_mac(PacketIn, OutPort, DatapathId, Opts) ->
    [InPort, DstMac] = packet_in_extract([in_port, dst_mac], PacketIn),
    Matches = [{in_port, InPort}, {eth_dst, DstMac}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer}]}],
    FlowOpts = [{table_id,0}, {priority, 100},
                {idle_timeout, get_opt(idle_timeout, Opts)},
                {hard_timeout, get_opt(hard_timeout, Opts)},
                {cookie, <<0,0,0,0,0,0,0,10>>},
                {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    FlowMod = of_msg_lib:flow_add(4, Matches, Instructions, FlowOpts),
    ok = exometer:update([flow_mod], 1),
    ok = ls_ctrl_client:send(DatapathId, FlowMod).

send_packet_out(DatapathId, Xid, PacketIn, OutPort) ->
    Actions = [{output, OutPort, no_buffer}],
    BufferId = packet_in_extract(buffer_id, PacketIn),
    PacketOut = construct_packet_out(BufferId, PacketIn, Actions),
    ok = exometer:update([packet_out], 1),
    ok = ls_ctrl_client:send(DatapathId, PacketOut#ofp_message{xid = Xid}).

packet_in_extract(Elements, PacketIn) when is_list(Elements) ->
    [packet_in_extract(H, PacketIn) || H <- Elements];
packet_in_extract(src_mac, PacketIn) ->
    <<_:6/bytes, SrcMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    SrcMac;
packet_in_extract(dst_mac, PacketIn) ->
    <<DstMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    DstMac;
packet_in_extract(in_port, PacketIn) ->
    <<InPort:32>> = proplists:get_value(in_port, proplists:get_value(match, PacketIn)),
    InPort;
packet_in_extract(buffer_id, PacketIn) ->
    proplists:get_value(buffer_id, PacketIn);
packet_in_extract(data, PacketIn) ->
    proplists:get_value(data, PacketIn).

construct_packet_out(no_buffer, PacketIn, Actions) ->
    [InPort, Data] = packet_in_extract([in_port, data], PacketIn),
    of_msg_lib:send_packet(4, Data, InPort, Actions);
construct_packet_out(BufferId, PacketIn, Actions) when is_integer(BufferId)->
    InPort = packet_in_extract(in_port, PacketIn),
    of_msg_lib:send_packet(4, BufferId, InPort, Actions).

format_mac(MacBin) ->
    Mac0 = [":" ++ integer_to_list(X, 16) || <<X>> <= MacBin],
    tl(lists:flatten(Mac0)).

update_handle_packet_in_metric(T1) ->
    DiffMicro = timer:now_diff(os:timestamp(), T1),
    exometer:update([app_handle_packet_in], DiffMicro).

get_opt(O, Opts) ->
    proplists:get_value(O, Opts).

remove_forwarding_entry(SrcMac, FwdTable) ->
    ls_metrics:update_fwd_table_size(maps:size(FwdTable)),
    maps:remove(SrcMac, FwdTable).

schedule_removing_entry(DatapathId, SrcMac) ->
    {ok, _Tref} = timer:send_after(
                    1000 * 30, {remove_forwarding_entry, DatapathId, SrcMac}).
