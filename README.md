# Clip
A library for creating an interactive command-line client for your Erlang application.  You implement your commands in a callback module similar to `gen_server`.

## Example
```erlang
%%% @doc An example of creating an interactive cli. This one keeps a simple count
%%% (state) where you can add, subtract, and show the current count.
-module(clip_simple).

%% Use the `clip_cli` behaviour
-behaviour(clip_cli).

-export([init/1, handle_command/2, start/0, usage/0, terminate/2]).

%% fire it up.
start() ->
    %% Make sure to call this with your callback module
    clip:start(?MODULE).

%% Call this to initialize your stuff before the cli starts.
%% We're using a simple count for state initially set to zero.
init(_Args) ->
    {ok, {count, 0}}.

%% Handle the `add` command.  It takes 1 argument which
%% must be a number > 0 and adds to the count. You'd
%% invoke this from the cli like this: `>>> add 1`
handle_command({add, [{number, N}]}, {count, V}) when N > 0 ->
    {ok, {count, V + N}};
%% Handle the `subtract` command.  It takes 1 argument which
%% must be a number > 0 and subtracts from the count. You'd
%% invoke this from the cli like this: `>>> subtract 5`
handle_command({subtract, [{number, N}]}, {count, V}) when N > 0 ->
    NewValue = V - N,
    case NewValue < 0 of
        true ->
            io:format(" error: count would be less then zero!\n"),
            {ok, {count, V}};
        _ ->
            {ok, {count, NewValue}}
    end;
%% Handle the `show` command. Displays the current state of the counter.
%% You'd invoke: `>>> show`.
handle_command({show, _}, {count, V}) ->
    io:format(" the current count is: ~w~n", [V]),
    {ok, {count, V}}.

%% Display the commands for help
usage() ->
    [
        "\tadd  <number>\n",
        "\t  Add to the current count\n",
        "\tsubtract <number>\n",
        "\t  subtract from the current count\n",
        "\tshow\n",
        "\t  show the current count"
    ].

%% Nothing to do here really.  Stopping the cli.
%% This callback is optional.
terminate(_, _) ->
    io:format("stopping!~n"),
    ok.
```

## Build
    rebar3 compile

## Run the example
    rebar3 shell
    > clip_simple:start().
    >>> help

