-module(tree).
-export([empty/0, insert/3, lookup/2, has_value/2, has_value2/2, addresses/0, test/0]).

empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).

has_value(_, {node, 'nil'}) ->
    false;
has_value(Val, {node, {_, Val, _, _}}) ->
    true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.

has_value2(Val, Tree) ->
    try has_value_with_throw(Val, Tree) of
        false -> false
    catch
        true -> true
    end.

has_value_with_throw(_, {node, 'nil'}) ->
    false;
has_value_with_throw(Val, {node, {_, Val, _, _}}) ->
    throw(true);
has_value_with_throw(Val, {node, {_, _, Left, Right}}) ->
    has_value_with_throw(Val, Left),
    has_value_with_throw(Val, Right).

addresses() ->
    T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()),
    T2 = tree:insert("Mark Anderson", "i.am.a@hotmail.com", T1),
    tree:insert("Anita Bath", "abath@someuni.edu",
        tree:insert("Kevin Robert", "myfairy@yahoo.com",
            tree:insert("Wilson Longbrow", "longwil@gmail.com", T2))).

test() ->
    Addresses = addresses(),
    io:format("has value of abath@someuni.edu: ~p~n", [has_value2("abath@someuni.edu", Addresses)]),
    io:format("has value of jreq@someuni.edu: ~p~n", [has_value2("jreq@someuni.edu", Addresses)]).
