%%%-------------------------------------------------------------------
%%% @author Szymon Mentel <>
%%% @copyright (C) 2015, Szymon Mentel
%%% @doc
%%%
%%% @end
%%% Created :  5 May 2015 by Szymon Mentel <>
%%%-------------------------------------------------------------------
-module(ls_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([handle_packet_in/2, handle_packet_out/2, update_fwd_table_size/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CTRL_HANDLE_PKT_IN, [controller_handle_packet_in]).
-define(APP_HANDLE_PKT_IN, [app_handle_packet_in]).
-define(SEC_TO_MILI(X), X * 1000).

-record(state, {times :: ets:tid()}).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle_packet_in(SwitchId,
                 #ofp_message{xid= Xid, type = packet_in, body = PacketIn}) ->
    #ofp_packet_in{buffer_id = BufferId} = PacketIn,
    Key = {SwitchId, Xid, BufferId},
    lager:debug("Saving timestamp for ~p", [Key]),
    gen_server:cast(?SERVER, {packet_in_arrived, Key, os:timestamp()});
handle_packet_in(_, _) ->
    ok.

update_fwd_table_size(Size) ->
    gen_server:cast(?SERVER, {fwd_table_size, Size}).

handle_packet_out(SwitchId, #ofp_message{xid = Xid, body = PacketOut})
  when element(1, PacketOut) =:= ofp_packet_out ->
    #ofp_packet_out{buffer_id = BufferId} = PacketOut,
    Key = {SwitchId, Xid, BufferId},
    lager:debug("Calculating timestamp for ~p", [Key]),
    gen_server:cast(?SERVER, {packet_in_handled, Key, os:timestamp()});
handle_packet_out(_, _) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ok = exometer_report:add_reporter(exometer_report_lager, []),
    setup_counters(),
    setup_histograms(),
    {ok, #state{times = ets:new(times, [])}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({packet_in_arrived, Key, T1}, #state{times = Times} = State) ->
    ets:insert(Times, {Key, T1}),
    {noreply, State};
handle_cast({packet_in_handled, Key, T2}, #state{times = Times} = State) ->
    DiffMicro = timer:now_diff(T2, ets:lookup_element(Times, Key, 2)),
    update_metric(?CTRL_HANDLE_PKT_IN, DiffMicro),
    ets:delete(Times, Key),
    {noreply, State};
handle_cast({fwd_table_size, Size}, State) ->
    exometer:update([fwd_table_size], Size),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = exometer:delete(?CTRL_HANDLE_PKT_IN),
    [ok = exometer:delete([T]) || T <- [flow_mod, packet_in, packet_out,
                                        app_handle_packet_in]].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_histograms() ->
    [begin
         Opts = report_values_aggregated_during_period(?SEC_TO_MILI(60)),
         %% histogram reports min, max, mean of values stored during a
         %% given timestamp. By deafault it also aggretates mean, max, min
         %% in a time slot ({slot_period, MILIS} option). So if time span is
         %% 60s and slot_period is 10s, histogram will return a mean of 10
         %% slot periods
         %% provided datapoints:
         %% max, min, mean, median, percentiles, number of values used in
         %% calculation
         ok = exometer:new(M, histogram, [Opts]),
         ok = exometer_report:subscribe(exometer_report_lager, M, [mean],
                                        _ReportInterval = ?SEC_TO_MILI(10))
     end || M <- [?CTRL_HANDLE_PKT_IN, ?APP_HANDLE_PKT_IN]],
    ok = exometer:new(M = [fwd_table_size], histogram,
                      [report_values_aggregated_during_period(?SEC_TO_MILI(60))]),
    ok = exometer_report:subscribe(exometer_report_lager, M, [mean, min, max],
                                   _ReportInterval = ?SEC_TO_MILI(10)).

setup_counters() ->
    [begin
         Opts = report_values_aggregated_during_period(?SEC_TO_MILI(60)),
         %% spiral is based on histogram but it returns a _sum_ of all
         %% values provided during a time span
         %% it prvides two datapoints:
         %% one - all values summed during a time span
         %% count - sum of ALL values
         ok = exometer:new([T], spiral, [Opts]),
         ok = exometer_report:subscribe(exometer_report_lager, [T], [one, count],
                                        _ReportInterval = ?SEC_TO_MILI(10))
     end || T <- [flow_mod, packet_in, packet_out]].

update_metric(Metric, DiffMicro) ->
    ok = exometer:update(Metric, DiffMicro).

report_values_aggregated_during_period(Micros) ->
    {time_span, Micros}.
