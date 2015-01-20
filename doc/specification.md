# Anond APIs and Protocol Messages

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
`get-random-nodes` and `reserve-oa`. This API is described in the [DS
JSON-RPC Server](ds-json-rpc-server.md) document. 

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
between the DS and nodes are described in the [DS-Node UDP
Protocol](ds-node-udp-protocol.md) document.

The actual traffic flowing between nodes over the encrypted tunnels is
described in the [Node-Node UDP Protocol](node-node-udp-protocol.md)
document.
