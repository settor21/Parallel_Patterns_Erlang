# Erlang Node Management Scripts

This repository contains scripts to manage Erlang nodes for distributed computing tasks.

## Scripts

### start_nodes.sh

This script starts a specified number of Erlang nodes on a given hostname.

#### Usage

```bash
$ ./start_nodes.sh -n <number_of_nodes> -h <hostname>
```

#### Arguments

-   `-n <number_of_nodes>`: The number of Erlang nodes to start.
-   `-h <hostname>`: The hostname where the nodes will be started.

#### Example

To start 3 nodes on `localhost`:

```bash
$ ./start_nodes.sh -n 3 -h localhost
```
### stop_nodes.sh

This script stops all active Erlang nodes that were started using the `start_nodes.sh` script.

#### Usage
```bash

$ ./stop_nodes.sh
```
#### Example

To stop all active Erlang nodes:

``` bash

$ ./stop_nodes.sh

```
Notes
-----

-   Ensure that `epmd` (Erlang Port Mapper Daemon) is running for node discovery. The `start_nodes.sh` script will check if `epmd` is running and start it if necessary.
-   The scripts assume that the Erlang nodes have names matching the pattern `node*`.