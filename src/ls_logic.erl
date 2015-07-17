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
                    {handle_packet_in, DatapathId, Xid, PacketIn, os:timestamp()}).

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
    ls_logic_common:init_exometer(),
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
handle_cast({handle_packet_in, DatapathId, Xid, PacketIn, T1},
            #state{switches = Switches0} = State) ->
    FwdTable0 = maps:get(DatapathId, Switches0),
    FwdTable1 = ls_logic_common:handle_packet_in(
                  DatapathId, Xid, PacketIn, FwdTable0, T1),
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
    ls_logic_common:terminate().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





