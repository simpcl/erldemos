-module(trace_file).

-export([start/0, start/1, init/1]).

-define(SEPARATOR, "=====================").


start() ->
    start([]).

start(Args) ->
    spawn(?MODULE, init, [Args]).

init(Args) ->
    Argc = length(Args),
    Pid = if
        Argc >= 2 ->
            [InFile | Tail] = Args,
            [OutFile | _] = Tail,
            io:format("Input File: ~p, Output File: ~p~n", [InFile, OutFile]),
            {ok, OutDevice} = file:open(OutFile, write),
            process_trace_file(InFile, OutDevice);
        Argc >= 1 ->
            %[First | _] = Args,
            %InFile = atom_to_list(First),
            [InFile | _] = Args,
            io:format("Input File: ~p~n", [InFile]),
            process_trace_file(InFile);
        true ->
            io:format("Invalid Args: ~p~n", [Args]),
            io:format("Usage: ./command <trace_file> <out_file>~n", []),
            init:stop()
    end,
    loop(Pid).

loop(Pid) ->
    timer:sleep(1000),
    case process_info(Pid) of
        undefined ->
            init:stop(),
            ok;
        Info ->
            io:format("~p~n", [Info]),
            loop(Pid)
    end.

process_trace_file(InFile) ->
    process_trace_file(InFile, group_leader()).

process_trace_file(InFile, OutDevice) ->
    io:format("trace client file: ~p, out device: ~p~n", [InFile, OutDevice]),

    Fun = fun(Msg, Ret) ->
        {Count} = Ret,
        %{ok, IoDevice} = file:open("/tmp/c.txt", [append, write]),
        process_trace_msg(OutDevice, Msg, Count),
        %file:close(IoDevice),
        {Count+1}
    end,    
    dbg:trace_client(file, InFile, {Fun, {0}}).

process_trace_msg(IoDevice, Msg, Count) ->
    io:format(IoDevice, "~s~n", [?SEPARATOR]),
    io:format(IoDevice, "Count: ~p~n", [Count]),
    case Msg of
        end_of_trace ->
            io:format(IoDevice, "Total Count: ~p~n", [Count]);
        _ ->
            io:format(IoDevice, "~p~n", [Msg])
    end.

