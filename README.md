# statman - Statistics man to the rescue!

Statman makes it possible to instrument your production Erlang systems
with very low overhead. Statman is inspired by Coda Hales metrics
(https://github.com/codahale/metrics/) and Boundarys Folsom
(https://github.com/boundary/folsom).

Statman takes advantage of efficient operations in ETS and does a few
tricks to save space and CPU. The result is that you can concurrently
collect latency histograms and retain the raw data for sound
statistical processing, without the space overhead of each sample. See
"How does it work" below.

The [statman_elli](https://github.com/knutin/statman_elli) project has
a real-time dashboard of statistics and HTTP endpointsfor retrieving
stats for external tools like Munin(plugin included), Librato,
Graphite, etc.

## Usage

Add `statman_server` to one of your supervisors:
```erlang
init([]) ->
    {ok, {{one_for_one, 5, 10}, [{statman_server, {statman_server, start_link, [1000]},
                                  permanent, 5000, worker, []}]}}.
```

From anywhere in your code:

```erlang
%% Counters measure the frequency of an event
statman_counter:incr(my_queue_in).

%% A gauge is a point in time snapshot of a value
statman_gauge:set(queue_size, N).

%% Histograms show you the distribution of values
Start = now(),
do_work()
statman_histogram:record_value(work_time, timer:now_diff(now(), Start)).
```

## How does it work

Using `ets:update_counter/3` we get very efficient atomic increments /
decrements of counters. With this primitive, counters, gauges and
histograms become very efficient.

A histogram is really a frequency table of values. By keeping a count
(weight) of how many times we have seen the different values, we have
enough information to calculate the mean, min, max, standard deviation
and percentiles.

Now, from this we can build something really cool:

 * The space required is proportionate to how many different values we
   have seen, not by the total number of observations. Binning values
   requires even less space.
 * Basic aggregation is done very early in the process. Binning also
   helps with this.
 * The frequency tables can easily be merged together, either to
   create an aggregate of multiple nodes to create a cluster view or
   aggregate over time to create for example 5 minute summaries.


## Clusters

In a single node application, you can collect, aggregate and push out
metrics from that single node. In bigger applications it might be
helpful to collect metrics inside of each node, but aggregate together
and view metrics for the whole cluster in one place. Having a "ops
dashboard" showing message queues in key processes, node throughput,
cluster throughput, request latency per node, request latency as a
whole, etc, is extremely useful.

## Setup

Statman has two parts, `statman_server` and `statman_aggregator`. The
server owns the ETS-tables and periodically forwards the changes to
any interested aggregator. The aggregator keeps a moving window of
metrics coming from one ore more servers. You can ask the aggregator
for the stats collected in the last N seconds.

You need to run one server under a supervisor in each node. If you
have a cluster of nodes, you can run the aggregator on just one of
them, collecting stats for the whole cluster.
