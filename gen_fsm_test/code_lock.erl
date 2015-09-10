-module(code_lock).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([button/1, open/2]).
-export([init/1, code_change/4, terminate/3, locked/2, handle_event/3, handle_sync_event/4, handle_info/3]).

-spec(start_link(Code::string()) -> {ok, pid()} | ignore | {error, term()}).
start_link([Code]) ->
    gen_fsm:start_link({local, code_lock}, code_lock, Code, []);
start_link(Code) ->
    gen_fsm:start_link({local, code_lock}, code_lock, Code, []).


-spec(button(Digit::string()) -> ok).
button(Digit) ->
    gen_fsm:send_event(code_lock, {button, Digit}).

open(timeout, State) ->
    do_lock(),
    {next_state, locked, State}.

%%--------------------
%% callback functions
%%--------------------
init(LockCode) ->
    io:format("init: ~p~n", [LockCode]),
    {ok, locked, {[], LockCode}}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, _StateName, _Data) ->
    ok.

locked({button, Digit}, {SoFar, Code}) ->
    io:format("button: ~p, So far: ~p, Code: ~p~n", [Digit, SoFar, Code]),
    InputDigits = lists:append(SoFar, Digit),
    io:format("InputDigits: ~p, Code: ~p~n", [InputDigits, Code]),
    case InputDigits of
        Code ->
            do_unlock(),
            {next_state, open, {[], Code}, 10000};
        Incomplete when length(Incomplete)<length(Code) ->
            {next_state, locked, {Incomplete, Code}, 5000};
        Wrong ->
            io:format("wrong passwd: ~p~n", [Wrong]),
            {next_state, locked, {[], Code}}
    end;
locked(timeout, {_SoFar, Code}) ->
    io:format("timout when waiting button inputting, clean the input, button again plz~n"),
    {next_state, locked, {[], Code}}.

handle_event(Event, StateName, Data) ->
    io:format("handle_event... ~n"),
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(Event, From, StateName, Data) ->
    io:format("handle_sync_event, for process: ~p... ~n", [From]),
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
    io:format("handle_info...~n"),
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

unexpected(Msg, State) ->
    io:format("~p RECEIVED UNKNOWN EVENT: ~p, while FSM process in state: ~p~n",
              [self(), Msg, State]).

do_unlock() ->
    io:format("passwd is right, open the DOOR.~n").

do_lock() ->
    io:format("over, close the DOOR.~n").

