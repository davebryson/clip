%%% @author Dave Bryson
%%% @doc Clip provides the basic functionality for a interactive command-line client.
%%% @end
-module(clip).

-export([start/1, start/2]).

-define(DEFAULT_PROMPT, " >>> ").

%% @doc Start the command line application
-spec start(module()) -> ok.
start(Mod) ->
    start(Mod, []).

%% @doc Start the command line application with the given state
-spec start(module(), term()) -> ok.
start(Mod, Args) ->
    {ok, State} = Mod:init(Args),
    loop(Mod, State).

%% Loop the callback Mod and it's state to gather input
loop(Mod, State) ->
    case io:get_line(?DEFAULT_PROMPT) of
        {error, _} ->
            shutdown(Mod, {error, io_lib:format(" io error~n", [])}, State);
        eof ->
            shutdown(Mod, {error, io_lib:format(" io error eof~n", [])}, State);
        Data ->
            Parsed = clip_leex:string(Data),
            process_input(Mod, Parsed, State)
    end.

%% Given the parsed input decide what to do
process_input(Mod, {ok, [{string, Cmd} | Args], _}, State) ->
    Command = list_to_atom(Cmd),
    case Command of
        'q' ->
            shutdown(Mod, normal, State);
        'quit' ->
            shutdown(Mod, normal, State);
        'help' ->
            try_usage(Mod),
            loop(Mod, State);
        Other ->
            case try_command(Mod, Other, Args, State) of
                {ok, {ok, NewState}} ->
                    loop(Mod, NewState);
                {stop, State} ->
                    shutdown(Mod, normal, State);
                {error, R, S} ->
                    io:format(" Error: ~w : ~p~n", [R, S]),
                    try_usage(Mod),
                    loop(Mod, State)
            end
    end;
process_input(Mod, {error, Reason, _}, State) ->
    shutdown(Mod, {error, io_lib:format(" Parsed failed: ~p~n", [Reason])}, State).

%% try to call the callback for a command
try_command(Mod, Cmd, Args, State) ->
    try
        {ok, Mod:handle_command({Cmd, Args}, State)}
    catch
        _:R:S ->
            {error, R, S}
    end.

%% shut it down ya'll
shutdown(Mod, Reason, State) ->
    case erlang:function_exported(Mod, terminate, 2) of
        true ->
            Mod:terminate(Reason, State),
            halt();
        false ->
            halt()
    end.

%% print usage
try_usage(Mod) ->
    io:format("\sUsage: <command> [args]~n"),
    io:format("~s~n", [Mod:usage()]).
