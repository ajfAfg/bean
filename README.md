# bean

bean is a Rebar3 plugin that automatically generates a supervision tree based on dependencies between processes. With this plugin, you can automatically generate a supervision tree that satisfies the following requirements simply by running this:

- The number of processes restarted at one time is minimal.
- All processes that depend on the terminated process are restarted.

Dependencies between processes are defined as the directions of communication between processes. For example, if process $p_1$ sends a message to process $p_2$, then "$`p_2`$ is dependent on $p_1$".

## Use

Add this plugin to your `rebar.config`:

```erlang
{plugins, [
    {bean, {git, "https://github.com/ajfAfg/bean.git", {tag, "0.1.0"}}}
]}.
```

Then just call the plugin directly in an existing application:

```sh
$ rebar3 bean
===> Fetching bean
===> Compiling bean
<Plugin Output>
```

See [the demo project](./demo) for a complete example of this plugin.

## Specifications

- The supervision tree supervise the processes defined under the floor of `src` directory.
- The supervisor source codes are output to `src/bean` directory.
- The name of the root in the supervision tree is `bean`.
- Supported restart strategies are `one_for_one`, `one_for_all`, and `rest_for_one` only.
  - i.e. `simple_one_for_one` is not supported.

## Limitations

Currently, bean has the following limitations:

- Only supports gen_server processes.
- Only `gen_server:call/2,3` and `gen_server:cast/2` communications are supported.
- Extractable communications are only those at the top level of the scope of each callback function of the module that implements gen_server.
  - It is also assumed that the first argument of each communication function is a literal representing the name of the gen_server.

## More information

The supervision tree generation algorithm implemented in this plugin is proven to output a certain optimal tree. In addition, experimental results show that the algorithm is fast enough for many realistic cases, although it takes the worst-case exponential time. See the master's thesis "[Automatic Generation of an Optimal Supervision Tree in Erlang]()" for details.

TODO: Add the link to the thesis when it is published.
