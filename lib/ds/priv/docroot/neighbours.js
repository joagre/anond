/*
  These examples did come handy:
  http://bl.ocks.org/mbostock/1095795
  http://bl.ocks.org/jhb/5955887
  http://bl.ocks.org/mbostock/4600693
  http://bl.ocks.org/mbostock/3750558
  Thanks!
*/

var width = 600;
var height = 600;

var nodes = [];
var links = [];

var selectedNodes = getParameterByName("selectedNodes");
selectedNodes = selectedNodes == null ? [] : selectedNodes.split("@");

function getParameterByName(name) {
    var match = RegExp("[?&]"+name+"=([^&]*)").exec(window.location.search);
    return match && decodeURIComponent(match[1].replace(/\+/g, ""));
}

var force = d3.layout.force()
    .nodes(nodes)
    .links(links)
    .size([width, height])
    .linkDistance(function(d) {
        return d["path-cost"]*5;
    })
    .on("tick", tick);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var node = svg.selectAll(".node");
var nodeLabel = svg.selectAll(".nodeLabel");
var link = svg.selectAll(".link");
var linkLabel = svg.selectAll(".linkLabel");

var networkTopology = null;

var redrawNeighbours = function() {
    anond.neighbours(networkTopology, selectedNodes, function(graph) {
        //console.log(graph.nodes);
        //console.log(nodes);
        
        // Add new nodes
        for (var i = 0; i < graph.nodes.length; i++)
            if (anond.nodeIndex(graph.nodes[i].na, nodes) == -1) {
	        console.log("Add new node: %s", graph.nodes[i].na);
                nodes.push(graph.nodes[i]);
            }
    
        // Add new link
        for (var i = 0; i < graph.links.length; i++) {
            var j = anond.linkIndex(graph.links[i].from, graph.links[i].to,
                                    links);
            if (j == -1) {
	        console.log("Add new link: %s -> %s",
		            graph.links[i].from, graph.links[i].to)
                links.push(graph.links[i]);
            } else {
	        console.log("Update existing link:  %s -> %s",
		            graph.links[i].from, graph.links[i].to)
                links[j]["path-cost"] = graph.links[i]["path-cost"];
                links[j].visible = graph.links[i].visible;
                links[j]["incoming-neighbour"] =
                    graph.links[i]["incoming-neighbour"];
            }
        }
        
        // Remove no longer existing links (http://billgonemad.com/blog/?p=34)
        for (var i = links.length-1; i >= 0; i--)
            if (anond.linkIndex(links[i].from, links[i].to,
                                graph.links) == -1) {
	        console.log("Remove no longer existing link: %s -> %s",
		            links[i].from, links[i].to);
                links.splice(i, 1);
            }

        // Remove no longer existing nodes (http://billgonemad.com/blog/?p=34)
        for (var i = nodes.length-1; i >= 0; i--)
            if (anond.nodeIndex(nodes[i].na, graph.nodes) == -1) {
	        console.log("Remove no longer existing node: %s", nodes[i].na);
                nodes.splice(i, 1);
            }

        redraw();
    });
};

var getNetworkTopology = function() {
    self = this;
    anond.getNetworkTopology(function(newNetworkTopology) {
        networkTopology = newNetworkTopology;
        redrawNeighbours();
        // NOTE: Drop automatic refresh. Add a manual button instead.
        //       Revert later.
        //setTimeout(getNetworkTopology, 4000);
    });
}

function redraw() {
    force.stop();

    link = link.data(force.links(), function(d) {
        return d.from+"-"+d.to;
    });
    link.enter()
        .insert("line", ".node")
        .attr("class", "link")
        .attr("marker-end","url(#arrowheadEnd)")
        .attr("marker-start", function(d) {
            if (d.biDirectional)
                return "url(#arrowheadStart)";
            else
                return null;
        });
    link.exit().remove();
    
    linkLabel = linkLabel.data(force.links(), function(d) {
        return d.from+"-"+d.to;
    });
    linkLabel.enter()
        .append("text")
        .attr("class", "linkLabel")
        .text(function(d) {
            return d["path-cost"];
        });
    linkLabel.exit().remove();    

    node = node.data(force.nodes(), function(d) {
        return d.na;
    });
    node.enter()
        .append("circle")
        .attr("r", 4)
        .attr("class", function(d) {
            return selectedNodes.indexOf(d.na) == -1 ? "node" : "selectedNode";
        })
        .on("dblclick", dblclick)
        .on("click", click)
        .call(force.drag());
    node.exit().remove();
    
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

    force.start();
}

var defs = svg.append("defs");

defs.append("marker")
    .attr({"id": "arrowheadEnd",
           "viewBox": "-0 -5 10 10",
           "refX": 15,
           "refY": 0,
           "orient": "auto",
           "markerWidth": 8,
           "markerHeight": 8,
           "xoverflow": "visible"})
    .append("svg:path")
    .attr("d", "M 0,-4 L 10,0 L 0,4")
    .attr("fill", "#ccc");

defs.append("marker")
    .attr({"id": "arrowheadStart",
           "viewBox": "-0 -5 10 10",
           "refX": -5,
           "refY": 0,
           "orient": "auto",
           "markerWidth": 8,
           "markerHeight": 8,
           "xoverflow": "visible"})
    .append("svg:path")
    .attr("d", "M 10,-4 L 0,0 L 10,4")
    .attr("fill", "#ccc");

function tick() {
    node
        .attr("cx", function(d) {
            return d.x;
        })
        .attr("cy", function(d) {
            return d.y;
        });
    
    nodeLabel
        .attr("x", function(d) {
            return d.x+7;
        })
        .attr("y", function(d) {
            return d.y+3;
        });
  
    link
        .attr("x1", function(d) {
            return d.source.x;
        })
        .attr("y1", function(d) {
            return d.source.y;
        })
        .attr("x2", function(d) {
            return d.target.x;
        })
        .attr("y2", function(d) {
            return d.target.y;
        })
        .style("stroke-dasharray", function(d) {
            if (d["incoming-neighbour"]) {
                return ("3, 3");
            } else
                return null;
        })
        .attr("visibility", function(d) {
            if (d.visible)
                return null;
            else
                return "hidden";
        });

    linkLabel
        .attr("x", function(d) {
            return d.source.x+(d.target.x-d.source.x)/2;
        })
        .attr("y", function(d) {
            return d.source.y+(d.target.y-d.source.y)/2;
        })
        .text(function(d) {
            return d["path-cost"];
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
        redrawNeighbours();
    }
}

function click(d) {
    if (d3.event.defaultPrevented)
        return;
    var i = selectedNodes.indexOf(d.na);
    if (i == -1) {
        d3.select(this).attr("class", "selectedNode");
        selectedNodes.push(d.na);
        redrawNeighbours();
    }
}

getNetworkTopology();
