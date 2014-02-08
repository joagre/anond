var anond = {};

anond.loadRoutes = function(loadHandler) {
    var self = this;
    var networkTopologyHandler = function(networkTopology) {
        var nodes = new Array();

        for (var i = 0; i < networkTopology.length; i++)
            nodes[i] = {"na": networkTopology[i].na};

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
                    var source = self.nodeIndex(from, networkTopology);
                    self._assert(source != -1, "source must not be -1");
                    var target = self.nodeIndex(to, networkTopology);
                    self._assert(target != -1, "target must not be -1");
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
                    self._assert(pathCost != null, "pathCost must not be null");
                    links[l++] = {"from": from,
                                  "to": to,
                                  "source": source,
                                  "target": target,
                                  "path-cost": pathCost};
                    from = to;
                }
            }

        loadHandler({nodes: nodes, links: links});
    };

    self._getNetworkTopology(networkTopologyHandler);
};

anond.neighbours = function(networkTopology, handler) {
    var nodes = new Array();

    for (var i = 0; i < networkTopology.length; i++)
        nodes[i] = {"na": networkTopology[i].na};

    var links = new Array();
    var k = 0;

    for (var i = 0; i < networkTopology.length; i++) {
        if(networkTopology[i].peers == null)
            continue;
//        var source = anond.nodeIndex(networkTopology[i].na, networkTopology);
        var source = anond._lookupNode(networkTopology[i].na, nodes);
        anond._assert(source != -1, "source must not be -1");
        for (var j = 0; j < networkTopology[i].peers.length; j++) {
            if(networkTopology[i].peers[j]["incoming-peer"])
                continue;
            else {
//                var target =
//                    anond.nodeIndex(networkTopology[i].peers[j].na,
//                                     networkTopology);
                var target = anond._lookupNode(networkTopology[i].peers[j].na,
                                               nodes);
                anond._assert(target != -1, "target must not be -1");
                links[k++] =
                    {
                        "source": source,
                        "target": target,
                        "from": networkTopology[i].na,
                        "to": networkTopology[i].peers[j].na,
                        "path-cost": networkTopology[i].peers[j]["path-cost"]
                    };
            }
        }
    }

    handler({nodes: nodes, links: links});
};

anond.getNetworkTopology = function(handler) {
    $.post("/jsonrpc",
           JSON.stringify({
               jsonrpc: "2.0",
               method: "get-network-topology",
               id: 1}),
           function(response) {
               if (response.result)
                   handler(response.result);
               else
                   if (response.error)
                       alert(response.error.message);
           },
           "json");
};

anond.nodeIndex = function(na, networkTopology) {
    for (var i = 0; i < networkTopology.length; i++)
        if (networkTopology[i].na == na)
            return i;
    return -1;
};

anond.linkIndex = function(from, to, links) {
    for (var i = 0; i < links.length; i++)
        if (links[i].from == from && links[i].to == to)
            return i;
    return -1;
};

/*
 * Helper functions
 */

anond._lookupNode = function(na, nodes) {
    for (var i = 0; i < nodes.length; i++)
        if (nodes[i].na == na)
            return nodes[i];
    return null;
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

anond._assert = function(condition, message) {
    if (!condition)
        $.error(msg);
};
