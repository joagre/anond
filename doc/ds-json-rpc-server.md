# The Directory Server JSON-RPC server

## 1) Overview

The *Directory Server* (DS) JSON-RPC server (from now on referred to as
the *server*) publish a number of methods to be used by nodes in order
for them to be a part of an Anond overlay network. The server provides
the central functionality provided by the *Public Directory Server* as
introduced in the paper [Anonymous overlay network supporting
authenticated routing](Schlegel-Wong-3.pdf) by Schlegel and Wong. 

The following methods are made available:

* [*publish-node*](#user-content-21-method-publish-node)
* [*unpublish-node*](#user-content-22-method-unpublish-node)
* [*still-published-nodes*](#user-content-23-method-still-published-nodes)
* [*get-random-nodes*](#user-content-24-method-get-random-nodes)
* [*reserve-oa*](#user-content-25-method-reserve-oa)
* [*get-network-topology*](#user-content-26-method-get-network-topology) (experimental API)

The server conforms to the [JSON-RPC 2.0
Specification](http://www.jsonrpc.org/specification) and uses HTTP
over SSL as its transport mechanism.

All incoming HTTP requests to the server must be signed using a
[Hash based Message Authentication
Code](http://en.wikipedia.org/wiki/Hash-based_message_authentication_code)
(HMAC). To bootstrap the HMAC signing scheme each node initially
publish its public signing key with a non-signed call to the
*publish-node* method.

The result returned from the call to the *publish-node* method is a
unique `node-id` (and more) and the node must send it along with each
HTTP request it sends to the server as a `Node-ID` HTTP header. After
this initial call the DS knows about the association between a
specific `node-id` and a public signing key.

From here on the node must sign all HTTP requests it sends to the
server and to do this it calculates a HMAC from the hashed body of the
HTTP request and sends this HMAC along with the HTTP request as a 
base64 encoded `Content-HMAC` HTTP header. 

There is more to this. Please read on.

## 2) The JSON-RPC methods

In this section each server method's input parameters and the result
it produces are specified using [JSON schema](http://json-schema.org)
specifications. Do not be scared. If your are new to JSON schema you
can sift through something like [Understanding JSON
Schema](http://spacetelescope.github.io/understanding-json-schema)  
instead of digging into the opaque JSON schema specification.

<!------------------------------------------------------------------------->

### 2.1) Method: *publish-node*

The *publish-node* method is used to publish a node in an Anond
overlay network.

#### Params:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "publish-node params",
    "type": "object",
    "properties": {
        "public-key": {
            "description":
                "A base64 encoded public signing key as defined in the
                NaCl library (http://nacl.cr.yp.to/sign.html), i.e. a
                key suitable for a signature scheme based on
                Curve25519 in Edwards form and SHA-512.",
            "type": "string",
            "media": {
                "binaryEncoding": "base64",
            }
        }
    },
    "required": ["public-key"]
}
```

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "publish-node result",
    "type": "object",
    "properties": {
        "ds-id": {
            "description":
                "A 'ds-id' is an id in the same number space as the
                node ids. DS picks this id in order to identify itself
                when it communicates with nodes using Anond's
                encrypted and bit oriented protocol used for node
                tunnel establishment etc. Read more about this bit
                protocol in ds-node-udp-protocol.md.",
            "type": "number",
            "minimum": 0,
            "maximum": 2147483647
        },
        "node-id": {
            "description":
                "If a node does not specify 'Content-HMAC' and
                'Node-ID' HTTP headers in the HTTP request the DS will
                allocate a new unique 'node-id' which the node should
                use to identify itself. If a node specify these HTTP
                headers the 'node-id' will be the same as specified in
                the 'Node-ID' header.",
            "type": "number",
            "minimum": 0,
            "maximum": 2147483647
        },
        "shared-key": {
            "description":
                "A base64 encoded shared stream key as defined in the
                NaCl library (http://nacl.cr.yp.to/stream.html),
                i.e. a key suitable for encryption based on a
                Salsa20/20 encryption scheme. This shared stream key
                is used by the node when it communicates with the DS
                using Anond's encrypted bit oriented protocol used for
                node tunnel establishment etc. Read more about this
                bit protocol in ds-node-udp-protocol.md",
            "type": "string",
            "media": {
                "binaryEncoding": "base64",
            }
        },
        "node-ttl": {
            "description":
                "A node must republish itself within this number of
                milli-seconds or else it will be purged from the Anond
                overlay network. A node republish itself with a new
                call to the 'publish-node' method and is then provided
                with a new shared stream key but it also has the
                opportunity to renegotiate a new public signing key
                with the DS. Note: If a node has forgotten its secret
                signing key (the companion to the public signing key)
                it has to call 'publish-node' again without any
                'Content-HMAC' and 'Node-ID' HTTP headers in the HTTP
                request, i.e. and it will get a new 'node-id'. The old
                'node-id' will not be given to any other node until a
                server defined grace period has passed (~two weeks).",
            "type": "number",
            "minimum": 900000
        }
    }
}
```

#### Error codes:

`DS_JSONRPC_BROKEN_SIMULATION` (7), `JSONRPC_PARSE_ERROR` (-32700),
`JSONRPC_INVALID_REQUEST` (-32600), `JSONRPC_METHOD_NOT_FOUND`
(-32601),  `JSONRPC_INVALID_PARAMS` (-32602), `JSONRPC_INTERNAL_ERROR`
(-32603)

#### Example:

First we generate a set of signing keys using `anond -j` and we also
assign a number of environment variables and a `curl` configuration
file to make life easier:

```
$ bin/anond -j public.key secret.key
$ cat public.key 
vOFs8Jc3zA8Rvax4Ot+kzNHIdcJZhvGAhioc/WoxD4Q=
$ cat secret.key
o0XiiJxdw8akZdg/NbXYyIT7HgEU4iSajoeI2OaDZxi84WzwlzfMDxG9rHg636TM0ch1wlmG8YCGKhz9ajEPhA==
$ PK=`cat public.key`
$ SK=`cat secret.key`
$ cat > curlrc
url="https://127.0.0.1:6700/jsonrpc"
header="Content-Type: application/json"
insecure
request="POST"
<Ctrl-D>
```

Then we issue an initial non-signed call to the *publish-node* method
without any `Node-ID` and `Content-HMAC` HTTP headers:

```
$ BODY='{"jsonrpc": "2.0", "method": "publish-node", "params": {"public-key": "'${PK}'"}, "id": 1}'
$ curl --config curlrc --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": {
    "ds-id": 472742719,
    "node-id": 22,
    "shared-key": "3CMFFX1G1ExgdNhYwB+JgCJ0A+VydTga9G5ZKEevXqw=",
    "node-ttl": 10800000
  },
  "id": 1
}
```

In return we got a `ds-id`, `node-id` and a `shared-key`. The
`node-ttl` tell us that we need to republish the node within 3 hours
or else the node will be purged from the Anond overlay network. 

To republish a node we call *publish-node* again and get a new
`shared-key` back as a side effect.

All method calls except for the initial call to *publish-node*
**must** be signed and the `Node-ID` and `Content-HMAC` HTTP headers
**must** be specified in the HTTP request. In this example we generate
a HMAC with `anond -l`:

```
$ echo -n ${BODY} > body.dat
$ HMAC=`bin/anond -l secret.key body.dat`
$ echo ${HMAC}
oSV4N9labmCL0dVzetktQtGbCikSA2Sl936bBEW39LUVJYqzVyQ+N9bSlNpKNre7vc6Ydq6DuMNg/2MHNiz/Ap8Hte3CRA/Cb997Esw+2MJpxF4Cgx9ekSxCHnh+7UcT4BeHQ3zRbLxjYlS7tv8UiTGKmt0+ygsffitoWF36e5k=
```

Then we republish the node:

```
$ curl --config curlrc -H "Node-ID: 22" -H "Content-HMAC: ${HMAC}" --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": {
    "ds-id": 472742719,
    "node-id": 22,
    "shared-key": "oxEQBqIOnbzJPKJJqbjgKoknw3R7Pvythvu8DEL052Q=",
    "node-ttl": 10800000
  },
  "id": 1
}
```

We got a new `shared-key` back and could have specified a new
`public-key` in the call in order to renegotiate a new public signing
key, but this is left out as an exercise to the reader.

<!------------------------------------------------------------------------->

### 2.2) Method: *unpublish-node*

The *unpublish-node* method is used to unpublish a node in an Anond
overlay network.

#### Params:

No parameters are needed, i.e. the `node-id` specified in the `Node-ID`
HTTP header in the HTTP request will be unpublished.

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "unpublish-node result",
    "description": "Signals if the unpublish was successful or not.",
    "type": "boolean"
}
```

#### Error codes:

`JSONRPC_PARSE_ERROR` (-32700),`JSONRPC_INVALID_REQUEST` (-32600),
`JSONRPC_METHOD_NOT_FOUND` (-32601), `JSONRPC_INVALID_PARAMS`
(-32602), `JSONRPC_INTERNAL_ERROR` (-32603)

#### Example:

```
$ BODY='{"jsonrpc": "2.0", "method": "unpublish-node"}, "id": 1}'
$ curl --config curlrc -H "Node-ID: 22" -H "Content-HMAC: ${HMAC}" --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": true,
  "id": 1
}
```

> The HMAC is calculated as seen in the
[*publish-node*](#user-content-example) example. The
[`curlrc`](#user-content-example) file is also defined there.

<!------------------------------------------------------------------------->

### 2.3) Method: *still-published-nodes*

The *still-published-nodes* method takes an array of `node-ids` as
input and returns an array of all `node-ids` that still are published.

#### Params:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "still-published-nodes params",
    "description":
        "An array of node-ids to be checked if they still are published.",
    "type": "array",
    "items": {
        "type": "number",
        "minimum": 0,
        "maximum": 2147483647
    },
    "uniqueItems": true
}
```

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "still-published-nodes result",
    "description": "An array of still published node-ids.",
    "type": "array",
    "items": {
         "type": "number",
         "minimum": 0,
         "maximum": 2147483647
    },
    "uniqueItems": true
}
```

#### Error codes:

`JSONRPC_PARSE_ERROR` (-32700), `JSONRPC_INVALID_REQUEST` (-32600),
`JSONRPC_METHOD_NOT_FOUND` (-32601), `JSONRPC_INVALID_PARAMS`
(-32602), `JSONRPC_INTERNAL_ERROR` (-32603)

#### Example:

```
$ BODY='{"jsonrpc": "2.0", "method": "still-published-nodes"}, "params": [1281, 3410, 52]}, "id": 1}'
$ curl --config curlrc -H "Node-ID: 22" -H "Content-HMAC: ${HMAC}" --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": [1281, 3410],
  "id": 1
}
```

> The HMAC is calculated as seen in the
[*publish-node*](#user-content-example) example. The
[`curlrc`](#user-content-example) file is also defined there.

<!------------------------------------------------------------------------->

### 2.4) Method: *get-random-nodes*

The *get-random-nodes* method returns an array of random `node-ids`
which are suitable to be used as neighbour nodes.

#### Params:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "get-random-nodes params",
    "description": "The number of random node-ids to return.",
    "type": "number",
    "minimum": 1,
    "required": true
}
```

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "get-random-nodes result",
    "description": "A random set of node-ids.",
    "type": "array",
    "items": {
         "type": "number",
         "minimum": 0,
         "maximum": 2147483647
    },
    "uniqueItems": true
}
```

#### Error codes:

`DS_JSONRPC_TOO_FEW_NODES`(3), `DS_JSONRPC_TOO_MANY_NODES`(5),
`DS_JSONRPC_BROKEN_SIMULATION` (7), `JSONRPC_PARSE_ERROR` (-32700),
`JSONRPC_INVALID_REQUEST` (-32600), `JSONRPC_METHOD_NOT_FOUND`
(-32601), `JSONRPC_INVALID_PARAMS` (-32602), `JSONRPC_INTERNAL_ERROR`
(-32603)

#### Example:

```
$ BODY='{"jsonrpc": "2.0", "method": "get-random-nodes"}, "params": 5}, "id": 1}'
$ curl --config curlrc -H "Node-ID: 22" -H "Content-HMAC: ${HMAC}" --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": [21212, 23121, 44439, 3882, 81819],
  "id": 1
}
```

> The HMAC is calculated as seen in the
[*publish-node*](#user-content-example) example. The
[`curlrc`](#user-content-example) file is also defined there.

<!------------------------------------------------------------------------->

### 2.5) Method: *reserve-oa*

The *reserve-oa* method reserves an *Overlay Address* (OA), i.e. an
ipv6-address from certain range, to be the node's address on the Anond
overlay network. A node typically registers several OAs and pick new
ones now and then to improve node anonymity.

#### Params:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "reserve-oa params",
    "description": "A random OA, i.e. a random ipv6-address in a given range.",
    "type": "string",
    "required": true
}
```

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "reserve-oa result",
    "description": "Signals if the reservation was successful or not."
    "type": "boolean"
}
```

#### Error codes:

`DS_JSONRPC_PERMISSION_DENIED` (1), `DS_JSONRPC_UNKNOWN_NODE` (2),
`JSONRPC_PARSE_ERROR` (-32700), `JSONRPC_INVALID_REQUEST` (-32600),
`JSONRPC_METHOD_NOT_FOUND` (-32601), `JSONRPC_INVALID_PARAMS`
(-32602), `JSONRPC_INTERNAL_ERROR` (-32603)

#### Example:

```
$ BODY='{"jsonrpc": "2.0", "method": "reserve-oa"}, "params": "fe80::230:48ff:fe33:bc33"}, "id": 1}'
$ curl --config curlrc -H "Node-ID: 22" -H "Content-HMAC: ${HMAC}" --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": true,
  "id": 1
}
```

> The HMAC is calculated as seen in the
[*publish-node*](#user-content-example) example. The
[`curlrc`](#user-content-example) file is also defined there.

<!------------------------------------------------------------------------->

### 2.6) Method: *get-network-topology*

The *get-network-topology* method can only be called if the
experimental API has been enabled in the DS. This method uses the bit
oriented [DS-Node UDP protocol](ds-node-udp-protocol.md) to extract
the neighbour nodes and routing entries from each node in the Anond
overlay network and then generates a global network topology. The
experimental API enables functionality which obviously defeats the
purpose of Anond but it is nice for experimentation and development
purposes.

> You have been duly warned.

#### Params:

No parameters are needed.

#### Result:

```json
{
    "$schema": "http://json-schema.org/draft-03/schema#",
    "name": "get-network-topology result",
    "description": "The network topology."
    "type": "array",
    "items": {
         "description":
             "An array of all nodes and their neighbour nodes and
             routing entries.",
         "type": "object",
         "properties": {
             "node-id": {
                 "type": "number",
                 "minimum": 1,
                 "maximum": 2147483647
             },
             "na": {
                 "description":
                     "The node address (NA), i.e. the node's
                     external/outside ipv4-address."
                 "type": "string"
             },
             "neighbours": {
                 "type": "array",
                 "items": {
                     "description": "An array of all neighbour nodes.",
                     "type": "object",
                     "properties": {
                         "node-id": {
                             "type": "number",
                             "minimum": 1,
                             "maximum": 2147483647
                         },
                         "na": {
                             "description":
                                 "The node address (NA), i.e. the
                                 node's external/outside published
                                 ipv4-address.",
                             "type": "string"
                         },
                         "path-cost": {
                             "description":
                                 "The path cost to reach this node.", 
                             "type": "number",
                             "minimum": 0,
                             "maximum": 65535
                         },
                         "incoming-neighbour": {
                             "description":
                                 "Signals if this node is chosen as an
                                 explicit neighbour node, or if another
                                 node has chosen this node as its
                                 neighbour node.",
                             "type": "boolean"
                         }
                     }
                 }
             },
             "route-entries": {
                 "type": "array",
                 "items": {
                     "description":
                         "An array of route entries, i.e. which nodes
                         has to be traversed in order to send data
                         from this node to all other nodes in the the
                         Anond overlay network.",
                     "type": "object",
                     "properties": {
                         "path-cost": {
                             "description":
                                 "The path cost to travel this route."
                             "type": "number",
                             "minimum": 0,
                             "maximum": 65535
                         },
                     "route": {
                         "type": "array",
                         "items": {
                             "description":
                                 "An array of node-ids constituting
                                 the route.",
                             "type": "number",
                             "minimum": 0,
                             "maximum": 65535
                         }
                     }
                 }
             }
         }
     }
}
```

#### Error codes:

`JSONRPC_PARSE_ERROR` (-32700), `JSONRPC_INVALID_REQUEST` (-32600),
`JSONRPC_METHOD_NOT_FOUND` (-32601), `JSONRPC_INVALID_PARAMS`
(-32602), `JSONRPC_INTERNAL_ERROR` (-32603)

#### Example:

```
$ BODY='{"jsonrpc": "2.0", "method": "get-network-topology", "id": 1}'
$ curl --config curlrc --data "${BODY}"
{
  "jsonrpc": "2.0",
  "result": [
    {
      "node-id": 2,
      "na": "127.0.0.1:50009",
      "neighbours": [
        {
          "node-id": 3,
          "na": "127.0.0.1:50010",
          "path-cost": 509,
          "incoming-neighbour": true
        },
        {
          "node-id": 5,
          "na": "127.0.0.1:50008",
          "path-cost": 502,
          "incoming-neighbour": false
        },
        {
          "node-id": 7,
          "na": "127.0.0.1:50005",
          "path-cost": 515,
          "incoming-neighbour": false
        }
      ],
      "route-entries": [
        {
          "path-cost": 1078,
          "route": [
            7,
            11
          ]
        },
        {
          "path-cost": 1026,
          "route": [
            7,
            9
          ]
        },
        {
          "path-cost": 898,
          "route": [
            5,
            10
          ]
        },
        {
          "path-cost": 1030,
          "route": [
            7,
            6
          ]
        },
        {
          "path-cost": 1450,
          "route": [
            5,
            10,
            4
          ]
        },
        {
          "path-cost": 524,
          "route": [
            7
          ]
        },
        {
          "path-cost": 396,
          "route": [
            5
          ]
        },
        {
          "path-cost": 560,
          "route": [
            3
          ]
        },
        {
          "path-cost": 831,
          "route": [
            5,
            8
          ]
        }
      ]
    },
    {
      "node-id": 3,
      "na": "127.0.0.1:50010",
      "neighbours": [
        {
          "node-id": 8,
          "na": "127.0.0.1:50004",
          "path-cost": 502,
          "incoming-neighbour": true
        }
        ...
      }
      ...
    }
    ...
  },
  "id": 1
}
```

> No HMAC is needed when calling methods in the experimental API. The
[`curlrc`](#user-content-example) file is as seen in the
[*publish-node*](#user-content-example) example.
