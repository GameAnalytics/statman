-module(statman_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = statman_sup:start_link([1000]),
    case application:get_env(statman, start_aggregator, true) of
        true ->
            statman_aggregator_sup:add_worker(statman_aggregator);
        false ->
            ok
    end,

    case application:get_env(statman, start_vm_metrics, true) of
        true ->
            init_vm_metrics();
        false ->
            ok
    end,
    {ok, Pid}.

stop(_State) ->
    ok.

init_vm_metrics() ->
    ok = statman_poller:add_gauge(fun statman_vm_metrics:get_gauges/0),
    ok = statman_poller:add_gauge(fun statman_vm_metrics:os_statss/0),
    ok = statman_poller:add_counter(fun statman_vm_metrics:get_counters/0).
