-module(matrix_multiplication).
-export([generate_matrix/3, multiply_matrices/2, time_matrix_multiplication/2]).

% Generate a matrix with random values between Lower and Upper
generate_matrix(Rows, Cols, {Lower, Upper}) ->
    lists:map(fun(_) -> generate_row(Cols, {Lower, Upper}) end, lists:seq(1, Rows)).

generate_row(Cols, {Lower, Upper}) ->
    lists:map(fun(_) -> rand:uniform(Upper - Lower + 1) + Lower - 1 end, lists:seq(1, Cols)).

% Multiply two matrices
multiply_matrices(Matrix1, Matrix2) ->
    lists:map(fun(Row) -> 
        lists:map(fun(Col) -> 
            dot_product(Row, Col) 
        end, transpose(Matrix2))
    end, Matrix1).

dot_product(Row, Col) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, Row, Col)).

transpose(Matrix) ->
    lists:map(fun(Idx) -> 
        lists:map(fun(Row) -> 
            lists:nth(Idx, Row) 
        end, Matrix) 
    end, lists:seq(1, length(hd(Matrix)))).

% Timing the matrix multiplication
time_matrix_multiplication(Matrix1, Matrix2) ->
    {Time, Result} = timer:tc(?MODULE, multiply_matrices, [Matrix1, Matrix2]),
    io:format("Matrix multiplication took ~p microseconds~n", [Time]),
    {Time, Result}.

%binary daa 
