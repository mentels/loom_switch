-module(ls_logic2).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, init_main_connection/1, terminate_main_connection/1,
         handle_packet_in/2, get_forwarding_table/1, clear_forwarding_table/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Type Definitions & Macros
%% ------------------------------------------------------------------

-type fwd_table() :: #{MacAddr :: string() => SwitchPort :: integer()}.
-record(state, {datapath_id :: binary(),
                fwd_table = #{} :: fwd_table(),
                logic_opts}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DatapathId, Opts) ->
    gen_server:start_link(?MODULE, [DatapathId, Opts], []).

init_main_connection(DatapathId) ->
    supervisor:start_child(ls_logic2_sup, [DatapathId]).

terminate_main_connection(Pid) ->
    supervisor:terminate_child(ls_logic2_sup, Pid).

handle_packet_in(Pid, {Xid, PacketIn}) ->
    exometer:update([packet_in], 1),
    gen_server:cast(Pid, {handle_packet_in, Xid, PacketIn, os:timestamp()}).

get_forwarding_table(Pid) ->
    gen_server:call(Pid, get_forwarding_table).

clear_forwarding_table(Pid) ->
    gen_server:cast(Pid, clear_forwarding_table).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Opts, DatapathId]) ->
    process_flag(trap_exit, true),
    lager:debug([{ls, x}], "[~p] Initialized loom switch logic2", [DatapathId]),
    {ok, #state{datapath_id = DatapathId, logic_opts = Opts}, 0}.


handle_call(get_forwarding_table, _From,
            #state{fwd_table = FwdTable} = State) ->
    {reply, {ok, FwdTable}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({handle_packet_in, Xid, PacketIn, T1},
            #state{datapath_id = DatapathId, fwd_table = FwdTable0,
                   logic_opts = Opts} = State) ->
    FwdTable1 = ls_logic_common:handle_packet_in(
                  DatapathId, Xid, PacketIn, FwdTable0, T1, Opts),
    {noreply, State#state{fwd_table = FwdTable1}};
handle_cast(clear_forwarding_table, #state{datapath_id = DatapathId} = State) ->
    lager:debug([{ls, x}], "[~p][clear_fwd_t] Cleared ~n",
                [DatapathId]),
    {noreply, State#state{fwd_table = #{}}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, #state{datapath_id = DatapathId} = State) ->
    ok = ofs_handler:subscribe(DatapathId, ls_ofsh2, packet_in),
    {noreply, State};
handle_info({remove_forwarding_entry, _, SrcMac},
            #state{datapath_id = DatapathId, fwd_table = FwdTable0} = State) ->
    FwdTable1 = ls_logic_common:remove_forwarding_entry(SrcMac, FwdTable0),
    lager:debug([{ls, x}], "[~p][del_entry] Removed fwd entry for ~p",
                [DatapathId, SrcMac]),
    {noreply, State#state{fwd_table = FwdTable1}}.


terminate(_Reason, #state{datapath_id = DatapathId}) ->
    case ofs_handler:unsubscribe(DatapathId, ls_ofsh2, packet_in) of
        ok ->
            ok;
        no_handler ->
            lager:debug([{ls, x}],
                        "[~p] ofs_handler died before unsubscribing packet_in",
                        [DatapathId])
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

