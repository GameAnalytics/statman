% We are now, by default, starting an aggregator (see statman_app.erl).
% The normal way of running Statman would be to have one aggregator running
% in your VM running Statman. Originally, it was designed so that you could
% start aggregator processes in a different VM and then add that process to
% another VM running Statman with statman_server:add_subscriber/2, but this
% is not a good approach as it does not follow OTP app principles.
-module(statman_aggregator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).
-export([add_worker/1]).

-spec start_link() -> ignore | {error, any()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_worker(Name :: atom()) -> {ok, pid()} | no_return().
add_worker(WorkerName) ->
    ChildSpec = worker_spec(WorkerName),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Reason} ->
            throw({unable_to_start_worker, WorkerName, Reason});
        {ok, Pid} ->
            {ok, Pid}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Children =
    case application:get_env(statman, start_aggregator, true) of
        true ->
            [worker_spec(statman_aggregator)];
        false ->
            []
    end,
    {ok, {{one_for_one, 5, 10}, Children}}.

worker_spec(Name) ->
    {Name, {statman_aggregator, start_link, []},
     permanent, 5000, worker, []}.
