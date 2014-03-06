var anond = {};

anond.routes = function(networkTopology, selectedNodes, handler) {
  var nodes = new Array();

  for (var i = 0; i < networkTopology.length; i++)
    nodes[i] = {"na": networkTopology[i].na};

  var links = new Array();
  var l = 0;

  for (var i = 0; i < networkTopology.length; i++) {
    var visible = true;
    if (selectedNodes.length != 0)
      if (selectedNodes.indexOf(networkTopology[i].na) == -1)
        visible = false;

    var startNode = anond._lookupNode(networkTopology[i].na, nodes);
    anond._assert(startNode != -1, "source must not be -1");

    for (var j = 0; j < networkTopology[i]["route-entries"].length; j++) {
      var path = new Array();
      path.push(startNode);

      var nextNode;

      for (var k = 0; k < networkTopology[i]["route-entries"][j].route.length; k++) {
        nextNode = anond._lookupNode(
          networkTopology[i]["route-entries"][j].route[k], nodes);
        anond._assert(nextNode != -1, "source must not be -1");
        path.push(nextNode);
      }

      links[l++] = {
        "source": startNode,
        "target": nextNode,
        "from": path[0].na,
        "to": path[path.length-1].na,
        "path-cost": networkTopology[i]["route-entries"][j]["path-cost"],
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
    nodes[i] = {"na": networkTopology[i].na};

  var links = new Array();
  var k = 0;

  for (var i = 0; i < networkTopology.length; i++) {
    if (networkTopology[i].peers.length == 0)
      continue;

    var source = anond._lookupNode(networkTopology[i].na, nodes);
    anond._assert(source != -1, "source must not be -1");

    for (var j = 0; j < networkTopology[i].peers.length; j++) {
      var link = anond._lookupLink(networkTopology[i].na,
                                   networkTopology[i].peers[j].na,
                                   links);
      if (link == null) {
        var target = anond._lookupNode(
          networkTopology[i].peers[j].na, nodes);
        anond._assert(target != -1, "target must not be -1");

        var visible = true;
        if (selectedNodes.length != 0)
          if (selectedNodes.indexOf(networkTopology[i].na) == -1)
            visible = false;

        var incomingPeer = false;
        if (selectedNodes.length != 0)
          if (selectedNodes.indexOf(networkTopology[i].na) != -1)
            incomingPeer = networkTopology[i].peers[j]["incoming-peer"];

        links[k++] = {
          "source": source,
          "target": target,
          "from": networkTopology[i].na,
          "to": networkTopology[i].peers[j].na,
          "path-cost": networkTopology[i].peers[j]["path-cost"],
          "visible": visible,
          "incoming-peer": incomingPeer
        };
      } else {
        link["path-cost"] =
          (link["path-cost"]+networkTopology[i].peers[j]["path-cost"])/2;
        link.biDirectional = true;

        link.visible = true;
        if (selectedNodes.length != 0)
          if (selectedNodes.indexOf(link.from) == -1 &&
              selectedNodes.indexOf(link.to) == -1)
            link.visible = false;

        if (selectedNodes.length != 0)
          if (selectedNodes.indexOf(networkTopology[i].na) != -1)
            link["incoming-peer"] = networkTopology[i].peers[j]["incoming-peer"];
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

anond._lookupLink = function(from, to, links) {
  for (var i = 0; i < links.length; i++)
    if ((links[i].from == from && links[i].to == to) ||
        (links[i].from == to && links[i].to == from))
      return links[i];
  return null;
};

anond._assert = function(condition, message) {
  if (!condition)
    $.error(msg);
};
