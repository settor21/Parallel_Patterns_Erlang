-module(parallel_matrix_mul).
-export([multiply/3]).

% Function to multiply two matrices in parallel
multiply(Matrix1, Matrix2, NumThreads) when is_integer(NumThreads), NumThreads > 0 ->
    % Validate matrix dimensions
    {_, Columns1} = matrix_dimensions(Matrix1),
    {Rows2, _} = matrix_dimensions(Matrix2),
    validate_dimensions(Columns1, Rows2),

    % Perform matrix multiplication in parallel
    ResultMatrix = parallel_multiply(Matrix1, Matrix2, NumThreads),

    % Return the resulting matrix
    ResultMatrix;
multiply(_, _, NumThreads) ->
    erlang:error({badarg, NumThreads}).

% Function to perform matrix multiplication in parallel
parallel_multiply(Matrix1, Matrix2, NumThreads) ->
    % Split Matrix1 into chunks for parallel processing
    ChunkSize = length(Matrix1) div NumThreads,
    Chunks = chunk_matrix(Matrix1, ChunkSize),

    % Spawn worker processes for each chunk
    Pids = lists:map(
        fun(Chunk) ->
            spawn(?MODULE, multiply_chunk, [self(), Chunk, Matrix2])
        end,
        Chunks
    ),

    % Receive results from worker processes
    Results = lists:map(
        fun(Pid) ->
            receive
                {Pid, ResultChunk} -> ResultChunk
            end
        end,
        Pids
    ),

    % Concatenate results chunks to form the final ResultMatrix
    lists:flatten(Results).

% Function to multiply a chunk of Matrix1 with Matrix2
multiply_chunk(ParentPid, Chunk, Matrix2) ->
    ResultChunks = lists:map(
        fun(Row1) ->
            lists:map(
                fun(Column2) ->
                    dot_product(Row1, Column2)
                end,
                transpose(Matrix2)
            )
        end,
        Chunk
    ),
    ParentPid ! {self(), ResultChunks}.

% Function to validate matrix dimensions
validate_dimensions(Columns1, Rows2) when Columns1 == Rows2 ->
    ok;
validate_dimensions(_, _) ->
    erlang:error(badarg).

% Function to extract matrix dimensions
matrix_dimensions(Matrix) ->
    {length(Matrix), length(hd(Matrix))}.

% Function to compute dot product of two lists
dot_product(List1, List2) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, List1, List2)).

% Function to transpose a matrix
transpose(Matrix) ->
    case Matrix of
        [] -> [];
        [FirstRow | RestRows] ->
            lists:foldl(
                fun(Elem, Acc) ->
                        lists:zipwith(fun(H, T) -> [H | T] end, Elem, Acc)
                end,
                lists:duplicate(length(FirstRow), []),
                [FirstRow | RestRows]
            )
    end.

% Function to chunk a list into smaller lists
chunk_matrix(List, ChunkSize) ->
    lists:reverse(chunk_matrix(List, ChunkSize, [])).

chunk_matrix([], _, Acc) ->
    Acc;
chunk_matrix(List, ChunkSize, Acc) when length(List) > ChunkSize ->
    {Chunk, Rest} = lists:split(ChunkSize, List),
    chunk_matrix(Rest, ChunkSize, [Chunk | Acc]);
chunk_matrix(List, _, Acc) ->
    [List | Acc].