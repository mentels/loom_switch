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
-export([of_message_in/1, of_message_out/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CTRL_HANDLE_PKT_IN, [controller_handle_packet_in]).

-record(state, {times :: ets:tid()}).

-include_lib("of_protocol/include/of_protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

of_message_in(#ofp_message{xid = Xid, type = packet_in}) ->
    gen_server:cast(?SERVER, {packet_in_arrived, Xid, erlang:now()});
of_message_in(_) ->
    ok.

of_message_out(#ofp_message{xid = Xid, body = PacketOut})
  when element(1, PacketOut) =:= ofp_packet_out ->
    gen_server:cast(?SERVER, {packet_in_handled, Xid, erlang:now()});
of_message_out(_) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    setup_measurement(?CTRL_HANDLE_PKT_IN),
    {ok, #state{times = ets:new(times, [])}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({packet_in_arrived, Xid, Now}, #state{times = Times} = State) ->
    ets:insert(Times, {Xid, Now}),
    {noreply, State};
handle_cast({packet_in_handled, Xid, Now}, #state{times = Times} = State) ->
    DiffMicro = timer:now_diff(Now, ets:lookup_element(Times, Xid, 2)),
    update_metric(?CTRL_HANDLE_PKT_IN, DiffMicro),
    ets:delete(Times, Xid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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
