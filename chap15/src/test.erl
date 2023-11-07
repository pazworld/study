% compile:
%   erl -make from command line
%   make:all([load]) from Eshell
% execute: erl -pa ebin

-module(test).
-export([test1/0, test2/0, test3/0]).
-record(state, {server,
                name="",
                to_go=0}).

%% event set after 5 seconds
test1() ->
    io:format("event set after 5 seconds, wait.~n"),
    spawn(event, loop, [#state{server=self(), name="test", to_go=[5]}]),
    receive
        X -> io:format("event received: ~p~n", [X])
    end.

%% event set after 500 seconds, then canceled
test2() ->
    io:format("event set after 500 seconds.~n"),
    Pid = event:start("Event", 500),
    timer:sleep(1000),
    io:format("event cancel.~n"),
    event:cancel(Pid).

%% event set after now + 3 seconds
test3() ->
    Now = erlang:localtime(),
    DateInSeconds = calendar:datetime_to_gregorian_seconds(Now),
    NewDateInSeconds = DateInSeconds + 3,
    SecondsAfter = calendar:gregorian_seconds_to_datetime(NewDateInSeconds),
    io:format("event set with start/2, wait 3 seconds.~n"),
    event:start("Event", SecondsAfter),
    receive
        X -> io:format("event received: ~p~n", [X])
    end.
