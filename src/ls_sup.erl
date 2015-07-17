-module(ls_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    BasicChildren = [?CHILD(ls_ctrl_client, worker),
                     ?CHILD(ls_metrics, worker)],
    Mode = application:get_env(ls, mode, regular),
    lager:info([{ls, x}], "Starting ls in ~p mode", [Mode]),
    Children = case  Mode of
                   proc_per_switch = Mode ->
                       application:set_env(ofs_handler, callback_module, ls_ofsh2),
                       [?CHILD(ls_logic2_sup, supervisor) | BasicChildren];
                   regular = Mode ->
                       [?CHILD(ls_logic, worker) | BasicChildren]
               end,
    {ok, {{one_for_one, 5, 10}, Children}}.

