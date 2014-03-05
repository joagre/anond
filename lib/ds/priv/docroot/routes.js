/*
These exemples did come handy:
http://bl.ocks.org/mbostock/1095795
http://bl.ocks.org/jhb/5955887
http://bl.ocks.org/mbostock/4600693
http://bl.ocks.org/mbostock/3750558
Thanks!
*/

var width = 960;
var height = 500;

var nodes = [];
var selectedNodes = [];
var links = [];

var force = d3.layout.force()
            .nodes(nodes)
            .links(links)
            .size([width, height])
            .linkDistance(function(d) {
              return d["path-cost"]*3;
            })
            .on("tick", tick);

var svg = d3.select("body").append("svg")
          .attr("width", width)
          .attr("height", height);

var node = svg.selectAll(".node");
var nodeLabel = svg.selectAll(".nodeLabel");
var link = svg.selectAll(".link");

var networkTopology = null;

var redrawRoutes = function() {
  anond.routes(networkTopology, selectedNodes, function(graph) {
    // Remove no longer existing nodes (http://billgonemad.com/blog/?p=34)
    for (var i = nodes.length-1; i >= 0; i--)
      if (anond.nodeIndex(nodes[i].na, graph.nodes) == -1)
        nodes.splice(i, 1);

    // Add new nodes
    for (var i = 0; i < graph.nodes.length; i++)
      if (anond.nodeIndex(graph.nodes[i].na, nodes) == -1)
        nodes.push(graph.nodes[i]);

    // Remove no longer existing links (http://billgonemad.com/blog/?p=34)
    for (var i = links.length-1; i >= 0; i--)
      if (anond.linkIndex(links[i].from, links[i].to, graph.links) == -1)
        links.splice(i, 1);

    // Add new link
    for (var i = 0; i < graph.links.length; i++) {
      var j = anond.linkIndex(graph.links[i].from, graph.links[i].to, links);

      if (j == -1)
        links.push(graph.links[i]);
      else {
        links[j]["path-cost"] = graph.links[i]["path-cost"];
        links[j].visible = graph.links[i].visible;
      }
    }

    start();
  });
};

var getNetworkTopology = function() {
  self = this;
  anond.getNetworkTopology(function(newNetworkTopology) {
    networkTopology = newNetworkTopology;
    redrawRoutes();
    setTimeout(getNetworkTopology, 2000);
  });
}

var curve = d3.svg.line()
           .x(function(d) {
             return d.x;
           })
           .y(function(d) {
             return d.y;
           })
           .interpolate("cardinal")

function start() {
  link = link.data(force.links(), function(d) {
           return d.from+"-"+d.to;
         });
  link.enter()
  .insert("path", ".node")
  .attr("class", "link");
  link.exit().remove();

  nodeLabel = nodeLabel.data(force.nodes(), function(d) {
                return d.na;
              });
  nodeLabel.enter()
  .append("text")
  .attr("class", "nodeLabel")
  .text(function(d) {
    return d.na;
  });
  nodeLabel.exit().remove();

  node = node.data(force.nodes(), function(d) {
           return d.na;
         });
  node.enter()
  .append("circle")
  .attr({"r": 4, "class": "node"})
  .on("dblclick", dblclick)
  .on("click", click)
  .call(force.drag());
  node.exit().remove();

  force.start();
}

function tick() {
  node.attr("cx", function(d) {
    return d.x;
  })
  .attr("cy", function(d) {
    return d.y;
  })

  nodeLabel.attr("x", function(d) {
    return d.x+7;
  })
  .attr("y", function(d) {
    return d.y+3;
  });

  link.attr("d", function(d) {
    return curve(d.path);
  })
  .attr("visibility", function(d) {
    if (d.visible)
      return null;
    else
      return "hidden";
  });
}

function dblclick(d) {
  var i = selectedNodes.indexOf(d.na);
  if (i != -1) {
    d3.select(this).attr("class", "node");
    selectedNodes.splice(i, 1);
    redrawRoutes();
  }
}

function click(d) {
  if (d3.event.defaultPrevented)
    return;
  var i = selectedNodes.indexOf(d.na);
  if (i == -1) {
    d3.select(this).attr("class", "selectedNode");
    selectedNodes.push(d.na);
    redrawRoutes();
  }
}

getNetworkTopology();
