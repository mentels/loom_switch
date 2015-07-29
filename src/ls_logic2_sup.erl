-module(ls_logic2_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type),
        {I, {I, start_link, [Args]}, permanent, 10000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LogicOpts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LogicOpts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LogicOpts]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(ls_logic2, LogicOpts, worker)]}}.

