-module(ls_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,
         start_link/0,
         stop/0,
         init_main_connection/1,
         handle_packet_in/2,
         get_forwarding_table/1,
         clear_forwarding_table/1,
         terminate_main_connection/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Type Definitions & Macros
%% ------------------------------------------------------------------

-type fwd_table() :: #{MacAddr :: string() => SwitchPort :: integer()}.
-type switches() :: #{DatapathId :: string() => ForwardingTable :: fwd_table()}.
-record(state, {switches = #{} :: switches()}).

-include_lib("of_protocol/include/of_protocol.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

init_main_connection(DatapathId) ->
    gen_server:cast(?SERVER, {init_main_connection, DatapathId}),
    lager:info([{ls, x}], "[~p] Initialized main connection", [DatapathId]).

handle_packet_in(DatapathId, {Xid, PacketIn}) ->
    exometer:update([packet_in], 1),
    gen_server:cast(?SERVER,
                    {handle_packet_in, DatapathId, Xid, PacketIn, erlang:now()}).

get_forwarding_table(DatapathId) ->
    gen_server:call(?SERVER, {get_forwarding_table, DatapathId}).

clear_forwarding_table(DatapathId) ->
    gen_server:cast(?SERVER, {clear_forwarding_table, DatapathId}).

terminate_main_connection(DatapathId) ->
    gen_server:cast(?SERVER, {terminate_main_connection, DatapathId}),
    lager:info([{ls, x}], "[~p] Terminated main connection", [DatapathId]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    init_exometer(),
    lager:debug([{ls, x}], "Initialized loom switch logic"),
    {ok, #state{}}.

handle_call({get_forwarding_table, DatapathId}, _From,
            #state{switches = Switches} = State) ->
    case maps:find(DatapathId, Switches) of
        error ->
            {reply, {error, unknow_datapath_id}, State};
        {ok, _FwdTable} = Reply ->
            {reply, Reply, State}
        end;
handle_call(stop, _From, #state{switches = Switches} = State) ->
    [ofs_handler:unsubscribe(DpId, ls_ofsh, packet_in)
     || DpId <- maps:keys(Switches)],
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({init_main_connection, DatapathId},
            #state{switches = Switches0} = State) ->
    Swtiches1 = maps:put(DatapathId, _FwdTable = #{}, Switches0),
    ok = ofs_handler:subscribe(DatapathId, ls_ofsh, packet_in),
    {noreply, State#state{switches = Swtiches1}};
handle_cast({terminate_main_connection, DatapathId},
            #state{switches = Switches0} = State) ->
    case ofs_handler:unsubscribe(DatapathId, ls_ofsh, packet_in) of
        ok ->
            ok;
        no_handler ->
            lager:debug([{ls, x}], "[~p] ofs_handler died before unsubscribing packet_in",
                        [DatapathId])
    end,
    Swtiches1 = maps:remove(DatapathId, Switches0),
    {noreply, State#state{switches = Swtiches1}};
handle_cast({handle_packet_in, DatapathId, Xid, PacketIn, Now0},
            #state{switches = Switches0} = State) ->
    FwdTable0 = maps:get(DatapathId, Switches0),
    FwdTable1  = learn_src_mac_to_port(DatapathId, PacketIn, FwdTable0),
    OutPort = case get_port_for_dst_mac(PacketIn, FwdTable0) of
                  undefined ->
                      flood;
                  PortNo ->
                      install_flow_to_dst_mac(PacketIn, PortNo, DatapathId),
                      lager:debug([{ls, x}], "[~p][flow] Sent flow mod", [DatapathId]),
                      PortNo
    end,
    send_packet_out(DatapathId, Xid, PacketIn, OutPort),
    update_handle_packet_in_metric(Now0),
    lager:debug([{ls, x}], "[~p][pkt_out] Sent packet out through port: ~p~n", [DatapathId,
                                                                                OutPort]),
    Switches1 = maps:update(DatapathId, FwdTable1, Switches0),
    {noreply, State#state{switches = Switches1}};
handle_cast({clear_forwarding_table, DatapathId},
            #state{switches = Switches} = State) ->
    case maps:find(DatapathId, Switches) of
        error ->
            lager:debug([{ls, x}], "[~p][clear_fwd_t] Failure: not connected~n",
                        [DatapathId]),
            {noreply, State};
        {ok, _FwdTable}  ->
            lager:debug([{ls, x}], "[~p][clear_fwd_t] Cleared ~n",
                        [DatapathId]),
            {noreply,
             State#state{switches = maps:put(DatapathId, #{}, Switches)}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    [ok = exometer:delete([T]) || T <- [flow_mod, packet_in, packet_out,
                                        handle_packet_in]],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

install_flow_to_dst_mac(PacketIn, OutPort, DatapathId) ->
    [InPort, DstMac] = packet_in_extract([in_port, dst_mac], PacketIn),
    Matches = [{in_port, InPort}, {eth_dst, DstMac}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer}]}],
    Opts = [{table_id,0}, {priority, 100},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    FlowMod = of_msg_lib:flow_add(4, Matches, Instructions, Opts),
    ok = exometer:update([flow_mod], 1),
    ok = ofs_handler:send(DatapathId, FlowMod).

send_packet_out(DatapathId, Xid, PacketIn, OutPort) ->
    [InPort, BufferId] = packet_in_extract([in_port, buffer_id], PacketIn),
    Actions = [{output, OutPort, no_buffer}],
    PacketOut = of_msg_lib:send_packet(4, BufferId, InPort, Actions),
    ok = exometer:update([packet_out], 1),
    ofs_handler:send(DatapathId, PacketOut#ofp_message{xid = Xid}).

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
    proplists:get_value(buffer_id, PacketIn).

format_mac(MacBin) ->
    Mac0 = [":" ++ integer_to_list(X, 16) || <<X>> <= MacBin],
    tl(lists:flatten(Mac0)).

init_exometer() ->
    [ok = exometer:new([T], spiral, [{time_span, 5000}])
     || T <- [flow_mod, packet_in, packet_out]],
    ok = exometer:new([app_handle_packet_in], histogram, [{time_span, 5000}]),
    ok = exometer_report:add_reporter(exometer_report_lager, []),
    ok = exometer_report:subscribe(
           exometer_report_lager, [packet_in], [one, count], 5200),
    ok = exometer_report:subscribe(
           exometer_report_lager, [app_handle_packet_in], [mean], 5200).

update_handle_packet_in_metric(Now0) ->
    DiffMicro = timer:now_diff(erlang:now(), Now0),
    exometer:update([app_handle_packet_in], DiffMicro).
