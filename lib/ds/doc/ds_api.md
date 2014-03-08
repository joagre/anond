# The directory server JSON-RPC API

The directory server is a JSON-RPC based HTTPS server which keeps
track of an anond overlay network. It provides a number of methods as
listed below.

## METHOD: get-number-of-peers

### Purpose
Returns number of nodes in overlay network

### Params
-

### Result
<number>

### Examples
`$ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-number-of-peers", "id": 1}' http://127.0.0.1:6700/jsonrpc`
```javascript
{
  "jsonrpc": "2.0",
  "result": 10,
  "id": 1
}
```

<!---------------------------------------------------------------------->

## METHOD:





    $ curl -X POST -d '{"jsonrpc": "2.0", "method": "enforce-peer-ttl", "id": 1}' http://127.0.0.1:6700/jsonrpc
    {
      "jsonrpc": "2.0",
      "result": true,
      "id": 1
    }

# get-peer

Purpose:
    Returns alla available information about a node

Params:
    <node-address>



Input:
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-all-peers", "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-random-peers", "params": {"my-na": "127.0.0.1:50010", "n": 4}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% curl -X POST -d '{"jsonrpc": "2.0", "method": "publish-peer", "params": {"na": "127.0.0.1:50011", "public-key": "aa", "flags": 4}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "unpublish-peer", "params": {"na": "127.0.0.1:50011"}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "published-peers", "params": {"nas": ["127.0.0.1:50010"]}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "reserve-oa", "params": {"oa": "fe80::c685:8ff:fe46:d502", "na": "127.0.0.1:50011"}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-network-topology", "id": 1}' http://192.168.1.80:6700/jsonrpc



%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "enforce-peer-ttl", "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-number-of-peers", "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-all-peers", "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-random-peers", "params": {"my-na": "127.0.0.1:50010", "n": 4}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% curl -X POST -d '{"jsonrpc": "2.0", "method": "publish-peer", "params": {"na": "127.0.0.1:50011", "public-key": "aa", "flags": 4}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "unpublish-peer", "params": {"na": "127.0.0.1:50011"}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "published-peers", "params": {"nas": ["127.0.0.1:50010"]}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "reserve-oa", "params": {"oa": "fe80::c685:8ff:fe46:d502", "na": "127.0.0.1:50011"}, "id": 1}' http://192.168.1.80:6700/jsonrpc
%% $ curl -X POST -d '{"jsonrpc": "2.0", "method": "get-network-topology", "id": 1}' http://192.168.1.80:6700/jsonrpc
