var anond = {};

anond.NODE_UNREACHABLE = 65535;

anond.routes = function(networkTopology, selectedNodes, handler) {
    var nodes = new Array();
    
    for (var i = 0; i < networkTopology.length; i++)
        nodes[i] = {"node-id": networkTopology[i]["node-id"],
                    "na": networkTopology[i].na};
    
    var links = new Array();
    var l = 0;
    
    for (var i = 0; i < networkTopology.length; i++) {
        var visible = true;
        if (selectedNodes.length != 0)
            if (selectedNodes.indexOf(networkTopology[i].na) == -1)
                visible = false;
        
        var startNode = anond._lookupNodeFromNa(networkTopology[i].na, nodes);
        anond._assert(startNode != null, "source must not be null");
        
        for (var j = 0; j < networkTopology[i]["route-entries"].length; j++) {
            if (networkTopology[i]["route-entries"][j]["path-cost"] ==
                anond.NODE_UNREACHABLE ||
                networkTopology[i]["route-entries"][j]["path-cost"] ==
                anond.NODE_UNREACHABLE-1)
                continue;
            
            var path = new Array();
            path.push(startNode);
            
            var nextNode;
            
            for (var k = 0;
                 k < networkTopology[i]["route-entries"][j].route.length;
                 k++) {
                nextNode = anond._lookupNodeFromNodeId(
                    networkTopology[i]["route-entries"][j].route[k], nodes);
                anond._assert(nextNode != null, "source must not be null");
                path.push(nextNode);
            }
            
            links[l++] = {
                "source": startNode,
                "target": nextNode,
                "from": path[0].na,
                "to": path[path.length-1].na,
                "path-cost":
                networkTopology[i]["route-entries"][j]["path-cost"],
                "path": path,
                "visible": visible
            };
        }
    }
    
    handler({nodes: nodes, links: links});
};

anond.neighbours = function(networkTopology, selectedNodes, handler) {
    var nodes = new Array();
    
    for (var i = 0; i < networkTopology.length; i++)
        nodes[i] = {"node-id": networkTopology[i]["node-id"],
                    "na": networkTopology[i].na};
    
    var links = new Array();
    var k = 0;
    
    for (var i = 0; i < networkTopology.length; i++) {
        if (networkTopology[i].neighbours.length == 0)
            continue;
        
        var source = anond._lookupNodeFromNa(networkTopology[i].na, nodes);
        anond._assert(source != null, "source must not be null ("+
                      networkTopology[i].na+", "+nodes+")");
        
        for (var j = 0; j < networkTopology[i].neighbours.length; j++) {
            if (networkTopology[i].neighbours[j]["incoming-neighbour"] ||
                networkTopology[i].neighbours[j]["path-cost"] ==
                anond.NODE_UNREACHABLE ||
                networkTopology[i].neighbours[j]["path-cost"] ==
                anond.NODE_UNREACHABLE-1)
                continue;
            
            var link = anond._lookupLink(
                networkTopology[i].neighbours[j].na, networkTopology[i].na,
                links);
            
            if (link == null) {
                var target = anond._lookupNodeFromNa(
                    networkTopology[i].neighbours[j].na, nodes);
                anond._assert(source != null, "target must not be null ("+
                              networkTopology[i].na+", "+nodes+")");
                var visible = true;
                if (selectedNodes.length != 0)
                    if (selectedNodes.indexOf(networkTopology[i].na) == -1 &&
                        selectedNodes.indexOf(
                            networkTopology[i].neighbours[j].na) == -1)
                        visible = false;
                
                var incomingNeighbour = false;
                if (selectedNodes.length != 0)
                    if (selectedNodes.indexOf(networkTopology[i].na) == -1)
                        incomingNeighbour = true;
                
                links[k++] = {
                    "source": source,
                    "target": target,
                    "from": networkTopology[i].na,
                    "to": networkTopology[i].neighbours[j].na,
                    "path-cost": networkTopology[i].neighbours[j]["path-cost"],
                    "visible": visible,
                    "biDirectional": false,
                    "incoming-neighbour": incomingNeighbour
                };
            } else {
                link["path-cost"] =
                    (link["path-cost"]+
                     networkTopology[i].neighbours[j]["path-cost"])/2;
                if (selectedNodes.length != 0)
                    if (selectedNodes.indexOf(link.from) == -1 && !link.visible)
                        link.visible = false;
                link.biDirectional = true;
                link["incoming-neighbour"] = false;
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

anond._lookupNodeFromNa = function(na, nodes) {
    for (var i = 0; i < nodes.length; i++)
        if (nodes[i].na == na)
            return nodes[i];
    return null;
};

anond._lookupNodeFromNodeId = function(nodeId, nodes) {
    for (var i = 0; i < nodes.length; i++)
        if (nodes[i]["node-id"] == nodeId)
            return nodes[i];
    return null;
};

anond._lookupLink = function(from, to, links) {
    for (var i = 0; i < links.length; i++)
        if (links[i].from == from && links[i].to == to)
            return links[i];
    return null;
};

anond._assert = function(condition, message) {
    if (!condition)
        $.error(message);
};
