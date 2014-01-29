var rpc = require('node-json-rpc');
var prettyjson = require('prettyjson');

var anond = {};

anond.init = function(host, port) {
    var options = {
        host: host,
        port: port,
        path: '/jsonrpc',
        strict: true
    };

    this._client = new rpc.Client(options);
    this._id = 0;
};

anond.load = function(loadHandler) {
    var self = this;
    var networkTopologyHandler = function(networkTopology) {
        var nodes = new Array();

        for (var i = 0; i < networkTopology.length; i++)
            nodes[i] = {"na": networkTopology[i].na};

        var neighbourLinks = new Array();
        var k = 0;

        for (var i = 0; i < networkTopology.length; i++) {
            if(networkTopology[i].peers == null)
                continue;
            var source =
                self._nodeIndex(networkTopology[i].na, networkTopology);
            console.assert(source != -1);
            for (var j = 0; j < networkTopology[i].peers.length; j++) {
                if(networkTopology[i].peers[j]["incoming-peer"])
                    continue;
                else {
                    var target =
                        self._nodeIndex(networkTopology[i].peers[j].na,
                                        networkTopology);
                    console.assert(target != -1);
                    neighbourLinks[k++] =
                    {"from": networkTopology[i].na,
                     "to": networkTopology[i].peers[j].na,
                     "source": source,
                     "target": target,
                     "path-cost": networkTopology[i].peers[j]["path-cost"]};
                }
            }
        }

        var links = new Array();
        var l = 0;

        for (var i = 0; i < networkTopology.length; i++)
            for (var j = 0; j < networkTopology[i]["route-entries"].length;
                 j++) {
                var from = networkTopology[i].na;
                for (var k = 0;
                     k < networkTopology[i]["route-entries"][j].length; k++) {
                    var to = networkTopology[i]["route-entries"][j][k];
                    if (self._linkMember(from, to, links))
                        break;
                    var source = self._nodeIndex(from, networkTopology);
                    console.assert(source != -1);
                    var target = self._nodeIndex(to, networkTopology);
                    console.assert(target != -1);
                    var fromPathCost =
                        self._lookupPathCost(from, to, networkTopology);
                    var toPathCost =
                        self._lookupPathCost(to, from, networkTopology);
                    var pathCost = null;
                    if (fromPathCost != null && toPathCost != null)
                        pathCost = (fromPathCost+toPathCost)/2;
                    else {
                        if (fromPathCost != null)
                            pathCost = fromPathCost;
                        else
                            pathCost = toPathCost;
                    }
                    console.assert(pathCost != null);
                    links[l++] = {"from": from,
                                  "to": to,
                                  "source": source,
                                  "target": target,
                                  "path-cost": pathCost};
                    from = to;
                }
            }

        loadHandler({nodes: nodes,
                     neighbourLinks: neighbourLinks,
                     links: links});
    };

    self._getNetworkTopology(networkTopologyHandler);
};

/*
 * Helper functions
 */

anond._nodeIndex = function(na, networkTopology) {
    for (var i = 0; i < networkTopology.length; i++)
        if (networkTopology[i].na == na)
            return i;
    return -1;
};

anond._linkMember = function(from, to, links) {
    for (var i = 0; i < links.length; i++)
        if ((links[i].from == from && links[i].to == to) ||
            (links[i].from == to && links[i].to == from))
            return true
    return false;
};

anond._lookupPathCost = function(from, to, networkTopology) {
    var peers = null;
    for (var i = 0; i < networkTopology.length; i++) {
        if (networkTopology[i].na == from) {
            peers = networkTopology[i].peers;
            break;
        }
    }
    if (peers == null)
        return null;
    for (var i = 0; i < peers.length; i++)
        if (peers[i].na == to)
            return peers[i]["path-cost"];
    return null;
};

anond._getNetworkTopology = function(resultHandler) {
    this._client.call({"jsonrpc": "2.0",
                       "method": "get-network-topology",
                       "id": this._id++},
                      function(error, response) {
                          if (error) {
                              console.log("error: %j\n", error);
                              console.assert(false);
                          } else
                              resultHandler(response.result);
                      });
};

/*
 * Test
 */

anond.init('127.0.0.1', 6700);

anond.load(function(graph) {
    console.log("nodes = %s\n", prettyjson.render(graph.nodes));
    console.log("neighbourLinks = %s\n",
                prettyjson.render(graph.neighbourLinks));
    console.log("links = %s\n", prettyjson.render(graph.links));
    console.log("neighbourLinks.length = %d\n", graph.neighbourLinks.length);
    console.log("links.length = %d\n", graph.links.length);
});
