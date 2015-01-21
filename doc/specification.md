# Anond APIs and Protocol Messages

An Anond overlay network is governed by a central directory server
(DS) handling node memberships and establishment of encrypted tunnels
between nodes etc. A DS is never in the data path between nodes
though, i.e. it just handles DS signalling.

A DS is a single point of failure but it is easy to setup a DS and
they can be as ephemeral as [Tor
bridges](https://www.torproject.org/docs/bridges.html.en),
i.e. ideally a multitude of DSs will running at any time.

N.B transparent bridging between different Anond overlay networks (and
its DSs) is a pending topic, as well as the removal of the single
point of failure characteristics. Do not hold your breath though. 

A DS makes a HTTPS/JSON-RPC API available which nodes use to join an
overlay network, i.e. the DS provides JSON-RPC methods such as
`publish-node`, `get-random-nodes` and `reserve-oa`. This API is
described in the [DS JSON-RPC Server](ds-json-rpc-server.md)
document. 

An node typically sits behind a NATing firewall and the DS uses [UDP
hole punching](http://en.wikipedia.org/wiki/UDP_hole_punching)
techniques in order to make nodes reachable from the DS and other
nodes.

An Anond node uses the DS JSON-RPC API to establish itself as a member
of the overlay network and after that point all Anond signalling is
multiplexed into the normal flow of underlying 512 bytes fixed size
encrypted UDP packets. The UDP based signalling protocol is described
in the [DS-Node UDP Protocol](ds-node-udp-protocol.md) document.

The actual traffic flowing between nodes over the encrypted tunnels is
described in the [Node-Node UDP Protocol](node-node-udp-protocol.md)
(**NOT YET WRITTEN**) document.
