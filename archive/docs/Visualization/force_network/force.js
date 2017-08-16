// Data
var graph = dta;
graphRec = JSON.parse(JSON.stringify(graph)); //Add this line

// Graph parameters
var width = 700;
var height = 600;

var color = d3.scale.category20();

var zoom = d3.behavior.zoom()
  .translate([0, 0])
  .scale(1)
  .scaleExtent([1, 8])
  .on("zoom", redraw);

var force = d3.layout.force()
  .charge(-45)
  .gravity(.25)
  .theta(.1)
  .alpha(1)
  .linkDistance(10)
  .size([width, height]);

var svg = d3.select("#sectionA")
  .append("svg")
  .attr("width", width)
  .attr("height", height)
  .on("dblclick", threshold)
  .attr("pointer-events", "all")
  .append('svg:g')
  .call(d3.behavior.zoom().on("zoom", redraw))
  .append('svg:g');

// Layer on a blank rectagle so graph can be panned
var rect = svg.append('svg:rect')
  .attr('class', "overlay")
  .attr('width', width)
  .attr('height', height)
  .attr('fill', 'white')
  .attr("opacity", .01)
  .call(zoom);

// Variable for dragging nodes
// Needed otherwise the rectangle will block dragging
var drag = force.drag()
  .origin(function(d) {
    return d;
  })
  .on("dragstart", dragstarted)
  .on("drag", dragged)
  .on("dragend", dragended);

// Start visual
force
  .nodes(graph.nodes)
  .links(graph.links)
  .start();

// Attributes for links
var link = svg.selectAll(".link")
  .data(graph.links)
  .enter().append("line")
  .attr("class", "link")
  .style("stroke-width", 1)
  .style("stroke", "#999")
  .style("marker-end", "url(#suit)");

// Attributes for nodes
var node = svg.selectAll(".node")
  .data(graph.nodes)
  .enter().append("circle")
  .attr("class", "node")
  .attr("r", 4)
  .attr("opacity", .75)
  .attr("stroke-opacity", 0)
  .call(force.drag)
  .style("fill", function(d) {
    return color(d.group);
  })
  .on("click", function(d) {
    d3.select("#pn").html(d.name);
    d3.select("#nn").html(d.nn);
    d3.select("#score").html(d.score);
    d3.select("#clust").html(d.group);
  });

// Assigns locations to nodes and links
force.on("tick", function() {
  link.attr("x1", function(d) {
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
    });
  d3.selectAll("circle").attr("cx", function(d) {
      return d.x;
    })
    .attr("cy", function(d) {
      return d.y;
    });
});

// Add arrows for the direction of each connection
function arrows() {
  svg.append("defs").selectAll("marker")
    .data(["suit", "licensing", "resolved"])
    .enter().append("marker")
    .attr("id", function(d) {
      return d;
    })
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 25)
    .attr("refY", 0)
    .attr("markerWidth", 45)
    .attr("markerHeight", 5)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5 L10,0 L0, -5")
    .on("threshold", redraw)
};


// Determines when nodes should be linked based on weight of similarity
function threshold(thresh) {
  graph.links.splice(0, graph.links.length);
  for (var i = 0; i < graphRec.links.length; i++) {
    if (graphRec.links[i].weight > thresh) {
      graph.links.push(graphRec.links[i]);
    }
  }
  d3.select("#MinScore-value").html(thresh)
  restart();
};

// Restart the visualisation after any node and link changes
function restart() {
  link = link.data(graph.links);
  link.exit().remove();
  link.enter().insert("line", ".node").attr("class", "link");
  node = node.data(graph.nodes);
  node.enter().insert("circle", ".cursor")
    .attr("class", "node")
    .attr("r", 5).call(force.drag);
  force.start();
  arrows();
};

// Restart the visualization after zooming or panning
function redraw() {
  svg.attr("transform",
    "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale +
    ")");
};

// Function for dragging nodes
function dragstarted(d) {
  d3.event.sourceEvent.stopPropagation();
  d3.select(this).classed("dragging", true);
};

function dragged(d) {
  d3.select(this).attr("cx", d.x = d3.event.x).attr("cy", d.y = d3.event.y);
};

function dragended(d) {
  d3.select(this).classed("dragging", false);
};
