%%% @doc Behaviour API for a CLI application
-module(clip_cli).

%% @doc Implement this to initiate anything in your application and possibly update state.
%% This is called before starting the CLI loop.
-callback init(Args :: term()) -> {ok, term()}.

%% @doc Implement for each command in your application. The `Input` to the function
%% must match the command and input arguments called from the terminal. For example,
%% If the command line input was:  `>>> add 1 2` you would implement:
%% `handle_command({add, [{number, A}, {number, b}], State)`.
%% ... talk about possible input value ...
-callback handle_command(Input :: {Command :: atom(), [{atom(), term()}]}, State :: term()) ->
    {ok, term()} | {stop, term()}.

%% @doc Return printable information about your commands. This information populates
%% the output of the `help` command.
-callback usage() -> [string()].

%% @doc This (optional) callback is invoked when the application is shutting down.
%% Use this hook to do your own app clean-up, etc....
-callback terminate(Reason :: normal | {error, string()}, State :: term()) -> 'ok'.

%% terminate is optional
-optional_callbacks([terminate/2]).
