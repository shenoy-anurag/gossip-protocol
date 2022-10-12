# gossip-protocol
Gossip protocol implementation using Erlang and Actor model.

## Team Members
- Anurag S

## What is working
- Creation of Supervisor.
- Creation of Actors.
- Assigning a topology (Line, 2D, 3D, Imperfect 3D, and Full).
- PushSum Algorithm.
- Gossip Protocol.

## Project Structure
### src
This folder contains the source files required to create the actors, supervisor and simulate the gossip protocol and push sum algorithm.

#### actors.erl
This file contains all the logic required to spawn actors, create a topology, assign actors to "locations" (to simulate real world topologies) and to execute the algorithms and protocols.

##### Functions
###### init
Receives three inputs: NumNodes, Topology, and Algorithm, from the user which are used to spawn the given number of actors, generate the topology, find neighbors for each actor, and execute the algorithm.

Example command is `actors:init(25, "3D", "PushSum").`, which will spawn 25 PushSum actors and create a 3D topology (3x3x3 = 27) and assigns the neighbours, which are 6 if ignoring diagonals (4 in the same plane, one above, and one below), so that the server/supervisor/master actor can start the algorithm.

###### determineTopologyDims
Determines the side length of the shape/solid which can accomodate the given number of nodes.

For example, 50 actors/nodes will fit into a 4x4x4 cube.

###### setupTopology
Creates the topology by determining neighbors of each actor, and then informing the actors about their neighbors.

###### spawnMultipleWorkers
Spawns the required number of actors of given type (Gossip or PushSum).

###### pushSumWorker
Push Sum worker/actor/node, which has the ability to Send messages to a neighbor, Receive messages from a neighbor, perform calculations, and interface with the server/supervisor when converged or terminated or for initial setup.

###### gossipWorker
Gossip worker/actor/node, which has the ability to Send messages to a neighbor, Receive messages from a neighbor, keep track of the number of times each "Rumor" is heard, and interface with the server/supervisor when converged or terminated or for initial setup.

Read more about the Gossip protocol here: <https://en.wikipedia.org/wiki/Gossip_protocol>

## Usage
### Main
1. Open a terminal and change directory to `src`.
2. Start erlang shell using `erl`.
3. Compile main file `c(actors).`.
4. Compile the supervisor `c(server).`.
5. Start the server/supervisor `server:init().`.
6. Initialize the actors `actors:init(10, "Line", "Gossip").`
7. Start a rumor by sending a message to the supervisor. Use `S = pid(0, 99, 0).` (if supervisor's pid is <0.99.0>) and `S ! {start_push_sum}.`.

### Commands in order
#### Terminal B
`erl -name b@127.0.0.1 -setcookie xyz`

`net_adm:ping('a@127.0.0.1').`

`c(server).`

`server:init().`

`S = pid(0, 99, 0).` Change as per Pid returned.
#### Terminal A
`erl -name a@127.0.0.1 -setcookie xyz`

`c(actors).`

`actors:init(25, "3D", "PushSum").`
#### Terminal B
`S ! {start_push_sum}.`

or

`S ! {start_gossip}.`


### Terminal A
`erl -name a@127.0.0.1 -setcookie xyz`

`c(actors).`

`actors:init(25, "3D", "PushSum").`

<!-- `P = pid(0, 99, 0).`

`P ! {share_gossip, "Rumor 1"}.`

`Delta = math:pow(10, -10).`

`P ! {start_push_sum, Delta}.` -->

### Terminal B
`erl -name b@127.0.0.1 -setcookie xyz`

`net_adm:ping('a@127.0.0.1').`

`c(server).`

`server:init().`

`S = pid(0, 99, 0).`

`S ! {start_push_sum}.`

`S ! {start_gossip}.`