sum_reduction
=====

An OTP application for summing huge 1D lists

## Setup
On Linux/Mac/WSL

### Single-node
Enter the src folder in the terminal
'''bash

    $ sudo apt install rebar3
    $ rebar3 compile
    $ rebar3 shell    
'''
Enter erl in the shell prompt

### Multi-node
First open the distributed_system directory in a new terminal
then

'''bash
    $ ./start_nodes.sh -n 3 -h localhost
'''

Enter the src folder in a new terminal
'''bash

    $ sudo apt install rebar3
    $ rebar3 compile
    $ rebar3 shell    
'''

Enter erl -name master@localhost -setcookie mycookie in the rebar3 prompt

## Test

### Summing Huge Lists of Numbers

#### Single Node

% Convert list to binary
List = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16].
Binary = convert_to_binary:list_to_binary(List).

% Sequential Sum
SequentialSum = sequential_sum:sum(Binary),
io:format("Sequential Sum: ~p~n", [SequentialSum]).

% Parallel Sum
ParallelSum = parallel_sum:sum(Binary),
io:format("Parallel Sum: ~p~n", [ParallelSum]).

% Multicore Reduce Sum
MulticoreReduceSum = reduce_sum:sum(Binary),
io:format("Multicore Reduce Sum: ~p~n", [MulticoreReduceSum]).

#### Multinode (Distributed System)

% Start Nodes
reduce_sum_distributed:start(['node2@hostname', 'node3@hostname']).

% Convert list to binary
List = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16].
Binary = convert_to_binary:list_to_binary(List).

% Distributed Sum
DistributedSum = reduce_sum_distributed:sum(Binary, ['node2@hostname', 'node3@hostname']),
io:format("Distributed Sum: ~p~n", [DistributedSum]).

If you want to stop it
'''bash
    $ ./stop_nodes.sh 
'''



