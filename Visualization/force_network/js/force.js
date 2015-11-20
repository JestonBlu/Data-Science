// Data
var graph = dta;
graphRec = JSON.parse(JSON.stringify(graph)); //Add this line

// Graph parameters
var width = 900,
  height = 600
  radius = 6;

var color = d3.scale.category20();

var zoom = d3.behavior.zoom()
  .translate([0, 0])
  .scale(1)
  .scaleExtent([1, 8])
  .on("zoom", redraw);

var force = d3.layout.force()
  .charge(-20)
  .gravity(.2)
  .linkDistance(10)
  .linkStrength(5)
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

// Add arrows for the direction of each connection
svg.append("defs").selectAll("marker")
  .data(["suit", "licensing", "resolved"])
  .enter().append("marker")
  .attr("id", function(d) {
    return d;
  })
  .attr("viewBox", "0 -5 10 10")
  .attr("refX", 25)
  .attr("refY", 0)
  .attr("markerWidth", 50)
  .attr("markerHeight", 2)
  .attr("orient", "auto")
  .append("path")
  .attr("d", "M0,-5L10,0L0,5 L10,0 L0, -5")
  .style("stroke", "gray")
  .style("opacity", "0.5")
  .on("threshold", redraw);


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
  .style("stroke-width", .3)
  .style("stroke", function(d){
    return color(d.type);
  })
  .style("marker-end", "url(#suit)");

// Attributes for nodes
var node = svg.selectAll(".node")
  .data(graph.nodes)
  .enter().append("circle")
  .attr("class", "node")
  .attr("r", 3)
  .attr("opacity", .45)
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
    d3.select("#pn_img").html(d.name);
    d3.select("#nn_img").html(d.nn);
    d3.select("#subcat").html(d.type)
    get_pn();
    get_nn();
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
};

// Restart the visualization after zooming or panning
function redraw() {
  svg.attr("transform",
    "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale +
    ")");
};

// Part Number Search - autocomplete
$(function() {
  var names = [];
  for (var i = 0; i < dta.nodes.length; i++) {
    names.push(dta.nodes[i].name);
  };
  $("#tags").autocomplete({
    source: names
  });
});

// Find the node in the network from part search input
function searchNode() {
  var selectedVal = document.getElementById('tags').value;
  var node = svg.selectAll(".node");
  if (selectedVal == "none") {
    node.style("stroke", "white").style("stroke-width", "1");
  } else {
    var selected = node.filter(function(d, i) {
      return d.name != selectedVal;
    });
    selected.style("opacity", "0");
    var link = svg.selectAll(".link")
    link.style("opacity", "0");
    d3.selectAll(".node, .link").transition()
      .duration(5000)
      .style("opacity", 1);
  }
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

function filter(arr, criteria) {
  return arr.filter(function(obj) {
    return Object.keys(criteria).every(function(c) {
      return obj[c] == criteria[c];
    });
  });
};

function get_pn() {
  var img = new Image();

  img.onload = function() {
    $("#pn_img_spot").remove()
    $("#files").append("<td id='pn_img_spot'> <img src=" + img.src + "></td>")
  };

  img.src = "images/Filters/" + $("#pn_img").text() + ".png"
}

function get_nn() {
  var img = new Image();

  img.onload = function() {
    $("#nn_img_spot").remove()
    $("#files").append("<td id='nn_img_spot'> <img src=" + img.src + "></td>")
  };

  img.src = "images/Filters/" + $("#nn_img").text() + ".png"
}
