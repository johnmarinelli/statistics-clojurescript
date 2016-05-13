var svg = d3.select('body').append('svg').attr('width', 500).attr('height', 500)
var dataset = [1,2,3,4,5]
var circles = svg.selectAll("circle")
                 .data(dataset)
                 .enter()
                 .append("circle");
circles.attr('cx', function(d,i) {return Math.random() * 720 }).attr('cy', 50).attr('r', 20)

