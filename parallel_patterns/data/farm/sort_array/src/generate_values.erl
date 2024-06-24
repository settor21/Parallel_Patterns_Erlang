-module(generate_values).
-export([generate_values/3, values_to_binary/1]).

-define(FILENAME, "generated_values.txt").

generate_values(LowerBound, UpperBound, NumValues) when LowerBound < UpperBound, is_integer(NumValues), NumValues > 0 ->
    Values = generate_random_values(LowerBound, UpperBound, NumValues),
    io:format("Generated ~B random values in range ~B to ~B.~n", [NumValues, LowerBound, UpperBound]),
    Values;
generate_values(_, _, _) ->
    io:format("Invalid parameters. Usage: generate_values(LowerBound, UpperBound, NumValues).~n").

generate_random_values(_, _, 0) ->
    [];
generate_random_values(LowerBound, UpperBound, NumValues) ->
    RandomNumber = rand:uniform(UpperBound - LowerBound + 1) + LowerBound - 1,
    [RandomNumber | generate_random_values(LowerBound, UpperBound, NumValues - 1)].

values_to_binary(Values) ->
    << <<Value:32/integer>> || Value <- Values >>.
