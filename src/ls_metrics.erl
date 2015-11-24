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

%% CPU and Memory monitoring
-export([cpu_utilization/0, mem_utilization/0]).

-define(SERVER, ?MODULE).
-define(CTRL_HANDLE_PKT_IN, [controller_handle_packet_in]).
-define(APP_HANDLE_PKT_IN, [app_handle_packet_in]).
-define(CPU, [cpu_avg]).
-define(MEM, [memory]).
-define(SEC_TO_MILI(X), X * 1000).

-record(state, {times :: ets:tid()}).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle_packet_in(_SwitchId, #ofp_message{type = packet_in} = Msg) ->
    Key = uuid:uuid4(),
    lager:debug("Saving timestamp for ~p", [Key]),
    gen_server:cast(?SERVER, {packet_in_arrived, Key, os:timestamp()}),
    Msg#ofp_message{xid = Key};
handle_packet_in(_, Msg) ->
    Msg.

update_fwd_table_size(Size) ->
    gen_server:cast(?SERVER, {fwd_table_size, Size}).

handle_packet_out(_SwitchId, #ofp_message{xid = Xid, body = PacketOut} = Msg)
  when element(1, PacketOut) =:= ofp_packet_out ->
    Key = Xid,
    lager:debug("Calculating timestamp for ~p", [Key]),
    gen_server:cast(?SERVER, {packet_in_handled, Key, os:timestamp()}),
    Msg#ofp_message{xid = 0};
handle_packet_out(_, Msg) ->
    Msg.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ok = exometer_report:add_reporter(exometer_report_lager, []),
    setup_counters(),
    setup_histograms(),
    {ok, #state{times = ets:new(times, [])}, 0}.

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

handle_info(timeout, State) ->
    initialize_cpu_and_memory_monitoring(),
    {noreply, State};
handle_info(cpu_utilization, State) ->
    cpu_utilization(),
    {noreply, State};
handle_info(mem_utilization, State) ->
    mem_utilization(),
    {noreply, State};
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
    Timespan = application:get_env(ls,
                                   histogram_time_span_in_micros,
                                   ?SEC_TO_MILI(60)),
    Opts = report_values_aggregated_during_period(Timespan),
    [
     begin
         %% histogram reports min, max, mean of values stored during a
         %% given time span. By deafault it also aggretates mean, max, min
         %% in a time slot ({slot_period, MILIS} option). So if time span is
         %% 60s and slot_period is 10s, histogram will return a mean of 10
         %% slot periods
         %% provided datapoints:
         %% max, min, mean, median, percentiles, number of values used in
         %% calculation
         ok = exometer:new(M, histogram, [Opts])
     end || M <- [
                  ?CTRL_HANDLE_PKT_IN,
                  ?APP_HANDLE_PKT_IN,
                  [fwd_table_size],
                  ?CPU,
                  ?MEM
                 ]
    ].

setup_counters() ->
    Timespan = application:get_env(ls,
                                   counter_time_span_in_micros,
                                   ?SEC_TO_MILI(60)),
    Opts = report_values_aggregated_during_period(Timespan),
    [begin
         %% spiral is based on histogram but it returns a _sum_ of all
         %% values provided during a time span
         %% it prvides two datapoints:
         %% one - all values summed during a time span
         %% count - sum of ALL values
         ok = exometer:new([T], spiral, [Opts])
     end || T <- [flow_mod, packet_in, packet_out]].

update_metric(Metric, DiffMicro) ->
    ok = exometer:update(Metric, DiffMicro).

report_values_aggregated_during_period(Micros) ->
    {time_span, Micros}.

initialize_cpu_and_memory_monitoring() ->
    CpuInterval = application:get_env(
                    ls,
                    cpu_utilization_check_interval,
                    ?SEC_TO_MILI(60)),
    MemInterval = application:get_env(
                    ls,
                    memory_utilization_check_interval,
                    ?SEC_TO_MILI(60)),
    {ok, _} = timer:send_interval(CpuInterval, cpu_utilization),
    {ok, _} = timer:send_interval(MemInterval, mem_utilization).

cpu_utilization() ->
    {all, BusyAvg, _, _} = cpu_sup:util([]),
    ok = exometer:update(?CPU, round(BusyAvg)).

mem_utilization() ->
    {_Total, AllocatedInBytes, _Worst} = memsup:get_memory_data(),
    ok = exometer:update(?MEM, AllocatedInBytes).










