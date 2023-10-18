% compile:
%   erl -make from command line
%   make:all([load]) from Eshell
% execute: erl -pa ebin

-module(test).
-export([test1/0]).
-record(state, {server,
                name="",
                to_go=0}).

test1() ->
    io:format("event set, wait 5 seconds.~n"),
    spawn(event, loop, [#state{server=self(), name="test", to_go=[5]}]),
    receive
        X -> io:format("event received:~p~n", [X])
    end.
