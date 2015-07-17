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
-export([handle_packet_in/2, handle_packet_out/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CTRL_HANDLE_PKT_IN, [controller_handle_packet_in]).

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
    setup_measurement(?CTRL_HANDLE_PKT_IN),
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
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = exometer:delete(?CTRL_HANDLE_PKT_IN),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_measurement(Metric) ->
    ok = exometer:new(Metric, histogram, [{time_span, 5000}]),
    ok = exometer_report:subscribe(
           exometer_report_lager, Metric, [mean], 5200).

update_metric(Metric, DiffMicro) ->
    ok = exometer:update(Metric, DiffMicro).
