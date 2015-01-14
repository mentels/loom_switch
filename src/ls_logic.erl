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
-type state() :: #state{}.

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
    lager:info("DPID: ~p initialized main connection", [DatapathId]).

handle_packet_in(DatapathId, Msg) ->
    gen_server:cast(?SERVER, {handle_packet_in, DatapathId, Msg}).

get_forwarding_table(DatapathId) ->
    gen_server:call(?SERVER, {get_forwarding_table, DatapathId}).

terminate_main_connection(DatapathId) ->
    gen_server:cast(?SERVER, {terminate_main_connection, DatapathId}),
    lager:info("DPID: ~p terminated main connection", [DatapathId]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
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
    ok = ofs_handler:unsubscribe(DatapathId, ls_ofsh, packet_in),
    Swtiches1 = maps:remove(DatapathId, Switches0),
    {noreply, State#state{switches = Swtiches1}};
handle_cast({handle_packet_in, DatapathId, Msg},
            #state{switches = Switches0} = State) ->
    FwdTable0 = maps:get(DatapathId, Switches0),
    FwdTable1  = learn_src_mac_to_port(Msg, FwdTable0),
    lager:debug("DPID ~p: added entry to fwd table: ~p", [FwdTable1]),
    %% OutPort = case get_port_for_dst_mac(Msg, FwdTable0) of
    %%               undefined ->
    %%                   flood;
    %%               PortNo ->
    %%                   install_flow_to_dst_mac(Msg, PortNo, DatapathId),
    %%                   PortNo
    %% end,
    %% send_packet_out(Msg, OutPort, DatapathId),
    %% lager:debug("DPID ~p: sent packet out through ~p~n", [DatapathId, OutPort]),
    Switches1 = maps:update(DatapathId, FwdTable1, Switches0),
    {noreply, State#state{switches = Switches1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

learn_src_mac_to_port(Msg, FwdTable0) ->
    InPort = proplists:get_value(in_port, proplists:get_value(match, Msg)),
    <<_DstMac:6/bytes, SrcMac:6/bytes, _/binary>> = proplists:get_value(data, Msg),
    maps:put(SrcMac, InPort, FwdTable0).

get_port_for_dst_mac(Msg, FwdTable) ->
    <<DstMac:6/bytes, _/binary>> = proplists:get_value(data, Msg),
    case maps:find(DstMac, FwdTable) of
        error ->
            undefined;
        {ok, Port} ->
            Port
    end.

install_flow_to_dst_mac(Msg, PortNo, DatapathId) ->
    ok.

send_packet_out(Msg, PortNo, DatapathId) ->
    ok.
    
    

%% [{buffer_id,261},
%%  {reason,no_match},
%%  {table_id,0},
%%  {cookie,<<0,0,0,0,0,0,0,0>>},
%%  {match,[{in_port,<<0,0,0,1>>}]},
%%  {data,
%%   <<255,255,255,255,255,255,50,159,179,
%%     220,238,226,8,6,0,1,8,0,6,4,0,1,50,
%%     159,179,220,238,226,10,0,0,1,0,0,0,0,
    %% 0,0,10,0,0,2>>}]}}
