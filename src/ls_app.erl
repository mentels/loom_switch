-module(ls_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    set_schedulers(),
    ls_sup:start_link().

stop(_State) ->
    ok.

set_schedulers() ->
    case application:get_env(ls, schedulers, default) of
        default ->
            ok;
        Num when is_integer(Num) ->
            erlang:system_flag(schedulers_online, Num),
            lager:info([{ls, x}], "Set number of schedulers online to ~p",
                       [Num])
    end.
