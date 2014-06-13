# Anond

## 1) Anonymous Routing

I warn you. Before you read any further. Be aware of the fact that I'm
almost completely clueless when it comes to almost any topic. If you
find something strange or just wrong in my thinking please do not be
upset. Be nice and put me on the right track instead.

That said.

Roman Schlegel and Duncan S. Wong, from the Department of Computer
Science City University of Hong Kong, have written a set of papers on
the topic of Anonymous Low Latency Routing.

Anond has implemented a version of these ideas and more.

The core idea is an overlay network of nodes which uses a low latency
anonymous routing protocol to hide the intermediate node hops a data
packet has to traverse in order to travel from a source node to a
destination node. Nodes in the overlay network only get to know about
the neighbour nodes that is best suited to forward a certain data
packet to a final destination node (going through a number of unknown
intermediary nodes). To facilitate this each node both have a
real-world ip-address as well as a number of unique overlay addresses
in the ipv6 range.

The routing protocol also ensures that a malign node member is unable
to attract a lot of traffic by just lying about its link capabilities.

You should at least read through the first of these papers before
going any further:

* [Anonymous overlay network supporting authenticated routing](Schlegel-Wong-3.pdf)
* [Low Latency High Bandwidth Anonymous Overlay Network with Anonymous Routing](Schlegel-Wong-1.pdf)
* [Monotonically Increasing Bit Vector for Authenticated Anonymous Routing(Schlegel-Wong-2.pdf)

The anond design documents focus on describing the API and protocol
messages flowing between nodes and the central directory server and
between the nodes themselves, i.e. a HTTPS/JSON-RPC API and a set of
encrypted UDP protocol messages.

## 2) Anond APIs and Protocol Messages

Each anond overlay network is governed by a central directory server
(DS) handling node membership and the establishment of encrypted
tunnels between nodes etc. A DS is indeed a single point of failure
but it is easy to setup a DS and they can be as ephemeral as Tor
bridges, i.e. ideally a multitude of anond DSs will be running at any
time and you just have to pick one that you and your friends like or 
indeed start your own, i.e. no data traffic will pass through a DS
potentially aggravating your service provider.

A DS makes a HTTPS/JSON-RPC API available which nodes use to join its
overlay network, i.e. it provides methods such as `publish-node`,
`get-random-nodes` and `reserve-oa`. This API is described in the (DS
JSON-RPC Server)[ds-json-rpc-server.md) document. 

An anond node typically sits behind a NATing firewall and its external
ip-address may vary over time and it can not just listen on a TCP (or
UDP) port without doing firewall reconfiguration. Nodes still need to
be reachable by the DS though in order for it to be able to
orchestrate the establishment of encrypted tunnels between nodes.

For this purpose anond defines a UDP based protocol which the DS and
the nodes uses to communicate. UDP was chosen over TCP to make it
feasible for both the DS and nodes to initiate a protocol message
exchange, without requiring firewall reconfigurations in the
nodes. This adds the need of [UDP hole punching]() but it fairly will
known well known technique these days. The protocol messages flowing
between the DS and nodes are described in the (DS-Node UDP
Protocol)[ds-node-udp-protocol.md) document.

The actual traffic flowing between nodes over the encrypted tunnels is
described in the (Node-Node UDP Protocol)[node-node-udp-protocol.md)
document.

## 2) Coding templates

Anond is written using the [Erlang](http://www.erlang.org) programming
language and uses all of its strengths when it comes to building large
scale network daemons. It also relies heavy on the Erlang OTP
principles to achieve fault tolerance.

The only Erlang programming pattern **not** used is the
[gen_server
behaviour](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
It hides Erlang's message passing mechanism and turns concurrent
programming into a boring slush of callback functions. It must be fun
to compute.

A number of Erlang [coding templates](coding_templates) have been used
to normalize the look and feel of the different types of Erlang modules
constituting the anond code base.
