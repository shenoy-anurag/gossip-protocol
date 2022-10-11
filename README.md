# gossip-protocol
Gossip protocol implementation using Erlang and Actor model.

## Team Members
- Anurag Shenoy

## What is working
- Creation of Supervisor.
- Creation of Actors.
- Assigning a topology (Line, 2D, 3D, Imperfect 3D, and Full).
- PushSum.
- Gossip.

## Usage
### Main
1. Open a terminal and change directory to `src`.
2. Start erlang shell using `erl`.
3. Compile main file `c(actors).`.
4. Initialize the actors `actors:init(10, "Line", "Gossip").`
5. Start a rumor by sending a message to a random actor. Use `P = pid(0, 99, 0).` and `P ! {share_gossip, "Rumor 1"}.`

#### On one machine, using two terminals
`erl -name a@127.0.0.1 -setcookie xyz`

`erl -name b@127.0.0.1 -setcookie xyz`

#### Connect the nodes
`net_adm:ping('b@127.0.0.1').`

#### Commands in order
`c(actors).`

`actors:init(20, "2D", "Gossip").`

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


### Correct sequence
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