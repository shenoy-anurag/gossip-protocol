# gossip-protocol
Gossip protocol implementation using Erlang and Actor model.

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

`P = pid(0, 99, 0).`

`P ! {share_gossip, "Rumor 1"}.`

`Delta = math:pow(10, -10).`

`P ! {start_push_sum, Delta}.`

### Terminal B
`erl -name b@127.0.0.1 -setcookie xyz`

`net_adm:ping('b@127.0.0.1').`

`c(server).`

`server:init().`


