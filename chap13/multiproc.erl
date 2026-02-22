-module(multiproc).
-compile([export_all]).

sleep(T) ->
    receive
    after T -> ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
        after 0 ->
            normal()
        end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
        after 0 ->
            []
        end.

important2() ->
    receive
        M = {Priority, _Message} when Priority > 10 ->
            [M | important2()]
        after 0 ->
            normal2()
        end.

normal2() ->
    receive
        M ->
            [M | normal2()]
        after 0 ->
            []
        end.

test() ->
    % show messages ordered by priority, arrival
    flush(),
    io:format("%%% send self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}~n"),
    self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high},
    io:format("%%% call important()~n"),
    M = important(),
    io:format("important() returns: ~p~n", [M]),
    ok.

test2() ->
    % show messages with priority ordered by priority, arrival
    flush(),
    io:format("%%% send self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}~n"),
    self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high},
    io:format("%%% call important2()~n"),
    M = important2(),
    io:format("important2() returns: ~p~n", [M]),
    ok.
