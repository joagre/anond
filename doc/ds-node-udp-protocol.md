# The DS-Node UDP protocol

## 1) Overview

The DS-Node UDP protocol is bit oriented and each protocol message is
encrypted within a 74 bytes cell. It provides a number of services
aimed at the member nodes of an anond overlay network:

* Node registration
* Node keepalive handling
* Node-node tunnel establishment
* Network topology extraction (experimental)

[UDP Hole punching](http://en.wikipedia.org/wiki/UDP_hole_punching) is
in the heart of this protocol.

The protocol messages are encrypted using a Salsa20/20 cipher as
described in http://nacl.cr.yp.to/stream.html, i.e. each cell starts
with a clear text node id (32 bits unsigned integer) and a clear text
client nonce (24 bytes) as described in
http://en.wikipedia.org/wiki/Cryptographic_nonce. The remaining 48
bytes is the actual protocol message and these can originate from the
the DS or from nodes.

The encryption is performed using a unique secret key which is shared
between each node and the DS. The shared secret key and each node's
unique node id are generated by the DS when the node presents itself to
the DS, i.e. when it calls the `publish-node` method provided by the
[DS JSON-RPC Server](ds-json-rpc-server.md).

A node typically republish itself, i.e. calls the `publish-node`
method repeatedly to renegotiate a new secret key to share with the
DS. The interval between secret key renegotiations must not exceed the
TTL returned by the `publish-node` method, or else the DS will purge
the node from the overlay network.

The DS listens on a UDP port which must be made available through the
DS's firewall (if any). Nodes typically reside behind residential
firewalls but there is no need to configure them to open up any
special UDP ports. Instead [UDP Hole punching
techniques](http://en.wikipedia.org/wiki/UDP_hole_punching) are used
to make these ports available.

The upcoming sections describe the possible protocol interactions
between the DS and nodes.

## 2) Node registration

The node registration is done by each node after it has called the
`publish-node` method, in the [DS JSON-RPC
Server](ds-json-rpc-server.md), and this informs the DS about the
external ip-address and UDP port which the node sits behind, i.e. a
node typically sits behind a NAT/firewall.

This external ip-address and UDP port is used by the DS when it needs
to orchestrate the establishment of an encrypted tunnels between nodes
(See "Node-node Tunnel Establishment" below), i.e. the external
node ip-addresses and UDP ports are used as endpoints for the
encrypted node-node tunnels. Note: The protocol messages sent between
nodes within these encrypted tunnels are described in a separate
[document](node-node-udp-protocol.md).

To register itself a node sends a `ds-register` message to the DS
until it gets a `node-registered` message in the return from the DS
with a matching message id. This is done each time nodes renegotiate
its shared secret key with the DS.

```
Node                            Directory Server                            Node
  ------------ ds-register ------------>
  ------------ ds-register ------------>
  ------------ ds-register ------------>
  ...
  <--------- node-registered -----------
```

### 2.1) Message: *ds-register* (74 bytes)

Direction: `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x00         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (42 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 2.2) Message: *node-registered*  (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x00         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (42 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 3) Node keepalive handling towards DS

Most nodes sit behind NAT/firewalls and the DS needs to be able to
reach the nodes on their external ip-addresses and UDP ports as
described in "Node registration" above.

To keep the node UDP port open in the node NAT/firewall the node must
send periodic keepalive messages to the DS (each ~10 seconds) each
renewing the life-time counters in the UDP state machine of the NAT, i.e
a [UDP Hole punching
technique](http://en.wikipedia.org/wiki/UDP_hole_punching). 

```
Node                            Directory Server                            Node
  ----------- ds-keepalive ------------>
  ----------- ds-keepalive ------------>
  ----------- ds-keepalive ------------>
...
```

### 3.1) Message: *ds-keepalive* (74 bytes)

Direction: `node -> [directory server]`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x01         |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (45 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 4) Tunnel Establishment

A node typically calls the 'get-random-nodes' method in [DS
JSON-RPC/HTTPS server](ds-json-rpc-server.md) to maintain a healthy
set of anond neighbour nodes.

A node must setup en encrypted tunnel with each neighbour node in
order to exchange both route entries, data packets. Note: The protocol
messages sent between nodes within these encrypted tunnels are
described in a separate [document](node-node-udp-protocol.md).

The DS is used as an intermediate to establish these tunnels, i.e. to
exchange the external endpoint ip-addresses and UDP ports for the
tunnel to the two nodes, but also to generate a secret key to be used
by the nodes to stream encrypt the tunnel protocol messages.

Each node renegotiate these tunnel secret keys at the same periodic
interval as it republish itself using the "publish-node" method in the
DS JSON-RPC server and issue a new `ds-register` protocol message to
the DS server.

For types oa messages are used to establish a tunnel:

```
Node                            Directory Server                            Node
  -------- ds-establish-tunnel -------->
                                        ------- node-establish-tunnel ------->
                                        <------- ds-tunnel-established -------
  <------ node-tunnel-established ------
```

A node issues a `ds-establish-tunnel` protocol message with a chosen
message id and then awaits a resulting `node-tunnel-established`
protocol message in return (with the same message id). It proceeds to
do this for ever until the appropriate `node-tunnel-established`
protocol message has arrived. Protocol message with stale message id's
are just ignored.

### 4.1) Message: *ds-establish-tunnel* (74 bytes)

Direction:  `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                          Src Node Id                                          |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x02         |                              Message ID                               |
|     32|  256|                                          Dest Node ID                                         |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                    Random Bytes (38 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.2) Message: *node-establish-tunnel* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x01         |                              Message ID                               |
|     32|  256|                                          Src Node ID                                          |
|     36|  288|                                         Src IP Address                                        |
|     40|  320|                Src Port Number                |
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                                      Shared Key (32 bytes)
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.3) Message: *ds-tunnel-established* (74 bytes)

Direction: `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                          Dest Node Id                                         |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x03         |                              Message ID                               |
|     32|  256|                                          Src Node ID                                          |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                     Random Bytes (38 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.4) Message: *node-tunnel-established* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x02         |                               Message ID                              |
|     32|  256|                                          Dest Node ID                                         |
|     36|  288|                                        Dest IP Address                                        |
|     40|  320|                Dest Port Number               |
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                                      Shared Key (32 bytes)
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 5) Get network topology (experimental)

In the [DS JSON-RPC/HTTPS server](ds-json-rpc-server.md) there is an
experimental `get-network-topology' method which, if called, goes out
to each and every node in the overlay network and ask about their
routing tables.

Nodes which have enabled experimental API support extends all route
entries with complete hop information for each route entry it knows
about, i.e. each route entry in the node's routing table contains
complete hop information on how a packet will traverse the overlay
network in order to reach a destination node.

Note: This defeats the whole idea of route anonymity but is nice in
order to setup an anond overlay network for route experimentation.

The following protocol messages are used by the DS to fetch the
routing tables for each each node:

```
Node                            Directory Server                            Node
  <----- node-get-network-topology -----
  -------- ds-network-topology -------->
  -------- ds-network-topology -------->
  -------- ds-network-topology -------->
  ...
```

### 5.1) Message: *node-get-network-topology* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x03         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                    Random Bytes (42 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 5.2) Message: *ds-get-network-topology* (< 512 bytes)

Direction: `node -> directory server`

HMMMM

A node's chosen neighbour nodes and its routing table (from now on
called network topology) contains a route entry for each member node in the overlay network, i.e. it can
become fairly large. For this reason the network topology is sent in
fragments (not larger than 480 bytes) when being sent to the DS.

For this purpose a fragment counter and a fragment size are
incorporated into this protocol message. The last bit in the fragment
counter signals if it was the last fragment to be sent and the
fragment size defines the size of the upcoming fragmen in the protocol 
message (obviously).

If duplicated or reordered `ds-get-network-topology` protocol messages
arrive it is up to the DS to sort them out, i.e. skip and/or
reorder. If a `ds-get-network-topology` protocol message disappears in
the network the extraction of the network topology table will fail,
i.e. there is no way to signal a retransmit of a
`ds-get-network-topology` protocol message fragment.

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x04         |                              Message ID                               |
|     32|  256|                Fragment Counter               |                 Fragment Size                 |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                            Fragment (Fragment Size bytes <= 480 bytes)
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|
...
|     508|4064|                                                                                              |
```

The fragments are put together by the DS and must be be decoded using
the following bit specification:

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                     Number of Neighbours                                      |

and as many neighbour nodes as "Number of Neighbours" indicates:

|      4|   32|                                            Node Id                                            |
|      8|   64|                                          IP Address                                           |
|     12|   96|                  Port Number                  |                   Path Cost                   |
|     16|  128|         Flags         |

...

The "Path Cost" 0xffff means that this node is unreachable and only one
bit level flag is defined:

HERE

* bit 1: Node is a an incoming node neighbour, i.e. another node has
  chosen me as his/her neighbour node.

|     52|  416|                                    Number of Route Entries                                    |

and as many route entries as "Number of Route Entries indicates:

|     56|  448|                  path-cost                    |                     Hop Size                  |

and as many "Node Id" hops as "Hop Size" indicates:

|     60|  480|                                       First Node Id Hop                                       |
|     64|  512|                                       Next Node Id Hop                                        |
|     68|  544|                                      And Next NodeId Hop                                      |
|     72|  576|                                              etc
...
```