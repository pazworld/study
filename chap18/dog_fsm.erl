-module(dog_fsm).
-export([start/0, squirrel/1, pet/1, release/1, test/0]).

start() ->
    spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.

pet(Pid) -> Pid ! pet.

release(Pid) -> Pid ! release.

bark() ->
    io:format("Dog says: BARK! BARK!~n"),
    receive
        pet ->
            wag_tail();
        _ ->
            io:format("Dog is confused~n"),
            bark()
    after 2000 ->
        bark()
    end.

wag_tail() ->
    io:format("Dog wags its tail~n"),
    receive
        pet ->
            sit();
        _ ->
            io:format("Dog is confused~n"),
            wag_tail()
    after 30000 ->
        bark()
    end.

sit() ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    receive
        squirrel ->
            bark();
        release ->
            io:format("Dog runs araw!~n"),
            ok;
        _ ->
            io:format("Dog is confused~n"),
            sit()
    end.

test() ->
    Pid = start(),
    io:format("> Dog barks every 2sec.~n"),
    timer:sleep(8000),
    io:format("> Pet~n"),
    pet(Pid),
    timer:sleep(1000),
    io:format("> Pet~n"),
    pet(Pid),
    timer:sleep(1000),
    io:format("> Dog find squirrel~n"),
    squirrel(Pid),
    timer:sleep(1000),
    io:format("> Pet~n"),
    pet(Pid),
    timer:sleep(1000),
    io:format("> Pet~n"),
    pet(Pid),
    timer:sleep(1000),
    io:format("> Release~n"),
    release(Pid).
