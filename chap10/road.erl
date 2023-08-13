-module(road).
-compile(export_all).

main() ->
    File = "road.txt",
    {ok, Bin} = file:read_file(File),
    Map = parse_map(Bin),
    {{SumA, ListA}, {SumB, ListB}} = lists:foldl(fun go_map/2, {{0, []}, {0,[]}}, lists:reverse(Map)),
    if SumA =< SumB -> ListA
    ;  SumA >  SumB -> ListB
    end.

parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Bin) when is_list(Bin) ->
    Values = [list_to_integer(X) || X <- string:tokens(Bin, "\r\n\t")],
    group_vals(Values, []).

group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([A,B,X|Rest], Acc) ->
    group_vals(Rest, [{A,B,X}|Acc]).

go_map({A, B, X}, {{SumA, ListA}, {SumB, ListB}}) ->
    NewA = if SumA =< SumB + X -> {SumA + A, [{a, A} | ListA]}
           ;  SumA >  SumB + X -> {SumB + X + A, [{a, A}, {x, X} | ListB]} end,
    NewB = if SumB =< SumA + X -> {SumB + B, [{b, B} | ListB]}
           ;  SumB >  SumA + X -> {SumA + X + B, [{b, B}, {x, X} | ListA]} end,
    {NewA, NewB}.

test() ->
    {{10, [{a, 10}]}, {15, [{b, 15}]}} = go_map({10, 15, 0}, {{0, []}, {0, []}}),
    {{15, [{a, 5}, {a, 10}]}, {14, [{b, 1}, {x, 3}, {a, 10}]}} = go_map({5, 1, 3}, {{10, [{a, 10}]}, {15, [{b, 15}]}}).
