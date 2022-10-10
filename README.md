# gossip-protocol
Gossip protocol implementation using Erlang and Actor model.

## Usage
### Main
1. Open a terminal and change directory to `src`.
2. Start erlang shell using `erl`.
3. Compile main file `c(actors).`.
4. Initialize the actors `actors:init(10, "Line", "Gossip").`
5. Start a rumor by sending a message to a random actor.