-module(cat_fsm).
-export([start/0, event/2, test/0]).

start() ->
    spawn(fun() -> dont_give_crap() end).

event(Pid, Event) ->
    Ref = make_ref(), % won't care for monitors here
    Pid ! {self(), Ref, Event},
    receive
        {Ref, Msg} -> {ok, Msg}
    after 5000 ->
        {error, timeout}
    end.

dont_give_crap() ->
    receive
        {Pid, Ref, _Msg} -> Pid ! {Ref, meh};
        _ -> ok
    end,
    io:format("Switching to 'dont_give_crap' state~n"),
    dont_give_crap().

test() ->
    Cat = start(),
    io:format("> pet~n"),
    event(Cat, pet),
    timer:sleep(1000),
    io:format("> love~n"),
    event(Cat, love),
    timer:sleep(1000),
    io:format("> cherish~n"),
    event(Cat, cherish),
    timer:sleep(1000),
    catch exit(Cat).
