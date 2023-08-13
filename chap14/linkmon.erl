-module(linkmon).
-compile(export_all).

flush(Timeout) ->
    receive
        Message -> io:format("Shell got ~p~n", [Message]),
        flush(0)
    after Timeout ->
        ok
    end.

myproc() ->
    timer:sleep(5000),
    exit(reason).

test() ->
    spawn(fun linkmon:myproc/0),
    io:format("spawn with no error~n"),
    link(spawn(fun linkmon:myproc/0)),
    io:format("spawn with link occur exception~n"),
    flush(6000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N - 1) end),
    link(Pid),
    receive
        _ -> ok
    end.

test2() ->
    link(spawn(linkmon, chain, [3])),
    io:format("chain will dead and occur exception~n"),
    flush(3000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
        {From, {"Range Against the Turing Machine", "Unit Testify"}} ->
            From ! {self(), "They are great!"};
        {From, {"System of a Downtime", "Memorize"}} ->
            From ! {self(), "They're no Johnny Crash but they're good."};
        {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {self(), "Simply incredible."};
        {From, {_Band, _Album}} ->
            From ! {self(), "They are terrible!"}
    end,
    critic().

test3() ->
    io:format("call critic and get answer about an album~n"),
    Critic = start_critic(),
    io:format("~p~n", [judge(Critic, "Genesis", "The Lambda Lies Down on Broadway")]),
    io:format("solar storm make critic disconnected, will timeout~n"),
    exit(Critic, solar_storm),
    io:format("~p~n", [judge(Critic, "Genesis", "A trick of the Tail Recursion")]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination, not a crash
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

judge2(Band, Album) ->
    critic ! {self(), {Band, Album}},
    Pid = whereis(critic),
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

unregister_critic() ->
    case whereis(critic) of
        undefined -> ok;
        _ -> unregister(critic)
    end.

test4() ->
    unregister_critic(),
    io:format("call critic and get answer about an album~n"),
    start_critic2(),
    timer:sleep(1000),
    io:format("~p~n", [judge2("Genesis", "The Lambda Lies Down on Broadway")]),
    io:format("solar storm make critic disconnected, but restart~n"),
    exit(whereis(critic), solar_storm),
    timer:sleep(1000),
    io:format("~p~n", [judge2("Genesis", "A trick of the Tail Recursion")]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

critic2() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic2().

start_critic3() ->
    spawn(?MODULE, restarter2, []).

restarter2() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination, not a crash
            ok;
        {'EXIT', Pid, _} ->
            restarter2()
    end.

judge3(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

test5() ->
    unregister_critic(),
    io:format("call critic and get answer about an album~n"),
    start_critic3(),
    timer:sleep(1000),
    io:format("~p~n", [judge3("Genesis", "The Lambda Lies Down on Broadway")]),
    io:format("solar storm make critic disconnected, but restart~n"),
    exit(whereis(critic), solar_storm),
    timer:sleep(1000),
    io:format("~p~n", [judge3("Genesis", "A trick of the Tail Recursion")]).
