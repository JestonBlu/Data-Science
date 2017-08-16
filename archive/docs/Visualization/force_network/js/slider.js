// when the input range changes update the value
d3.select("#MinScore").on("input", function() {
  update(+this.value);
});

// Initial starting radius of the circle
update(0);

// update the elements
function update(MinScore) {
  d3.select("#MinScore-value").text(MinScore);
  d3.select("#MinScore").property("value", MinScore);
};
