-module(npc).
 
-behaviour(gen_fsm).
 
%% API
-export([start_link/0]).
 
-export([hero_join/0, hero_leave/0, hero_event/1, hero_sync_event/1, hero_sync_event/2]).
 
%% gen_fsm callbacks
-export([init/1, static/2, moving/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
-define(SERVER, ?MODULE).
 
-record(npc, {state}).
 
start_link() ->
    Result = gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []),
    io:format("start_link, Result:~p~n", [Result]),
    Result.


hero_join() ->
    gen_fsm:send_event(?SERVER, hero_join).
 
hero_leave() ->
    gen_fsm:send_event(?SERVER, hero_leave).

hero_event(Event) ->
    gen_fsm:send_all_state_event(?SERVER, Event).

hero_sync_event(Event) ->
    gen_fsm:sync_send_all_state_event(?SERVER, Event).

hero_sync_event(FsmRef, Event) ->
    Result = gen_fsm:sync_send_all_state_event(FsmRef, Event),
    io:format("hero_sync_event, Result:~p~n", [Result]),
    Result.


init([]) ->
    State = #npc{state = static},
    io:format("init, State: ~p~n", [State]),
    {ok, static, State}.
 
static(Event, State) ->
    case Event of
    hero_join ->
        do_moving(),
        NewState = State#npc{state = moving},
        io:format("npc set state: ~p~n", [NewState]),
        {next_state, moving, NewState}
    end.
 
moving(Event, State) ->
    case Event of
    hero_leave ->
        do_static(),
        NewState = State#npc{state = static},
        io:format("npc set state: ~p~n", [NewState]),
        {next_state, static, NewState}
    end.
 
handle_event(Event, StateName, State) ->
    io:format("handle_event, Event:~p, StateName:~p\n", [Event, StateName]),
    {next_state, StateName, State}.
 
handle_sync_event(Event, From, StateName, State) ->
    io:format("handle_sync_event, Event:~p, From:~p, StateName:~p\n", [Event, From, StateName]),
    gen_fsm:reply(From, {ok, <<"reply data">>}),
    io:format("after gen_fsm:reply~n", []),
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
 
do_moving() ->
    io:format("npc beigin moving...~n").
 
do_static() ->
    io:format("npc stop moving, join static...~n").
