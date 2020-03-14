function drawCyto(cortype, testtype, two, datfile, whichrawdat, whichlayout){
	// References:
	// 1: https://stackoverflow.com/questions/31510992/how-to-highlight-neighbouring-nodes-in-cytoscape-js
	// 2: https://stackoverflow.com/questions/49156044/highlighting-edges-with-cytoscape-js-doesnt-work
	scatprompt.selectAll('g').remove();
	corrplot.selectAll('g').remove();
	scatterplot1.selectAll('g').remove();
	scatterplot2.selectAll('g').remove();

	if (is_X_Y) {
		node_color_style = [{
			selector: '.X',
			style: {
				'background-color': 'orange',
			}
		},
		{
			selector: '.Y',
			style: {
				'background-color': 'green',
			}
		},
		{
            selector: '.X.highlight',
            style: {
                'background-color': 'red',
            }
        },
        {
            selector: '.Y.highlight',
            style: {
                'background-color': 'blue',
            }
        }
		]
	} else {
		node_color_style = [{
			selector: 'node',
			style: {
				'background-color': 'gray'
			}
		},
		{
            selector: 'node.highlight',
            style: {
                'background-color': 'red',
            }
        }]
	}
	var graph_name = two ? ("graph_diff_"+cortype+"_"+testtype) : ("graph_"+cortype+"_"+testtype+"_"+whichrawdat),
		edge_name = graph_name + "_edges",
		max_degree = parseInt(window[graph_name + "_maxDegree"], 10);

	console.log(graph_name);
	console.log(window[edge_name]);
	console.log(graph_nodes);

	/*if (typeof window[edge_name] === 'undefined'){
		corrplot.append('g').append('text')
			.text(testtype === "raw" ? "Graphs not supported for raw correlations. Please change to other test types." : "Data not available.")
			.attr({
				'class': 'scatterlabel',
				'x': full_width/2,
				'y': h/2,
				'text-anchor': 'middle',
				'dominant-baseline': 'middle'
			})
			.style("font-size", labelsize+"px");
	}*/
    var cy = cytoscape({
  		container: document.getElementById('cy'),
  		elements: graph_nodes.concat(window[edge_name]),
    	style: node_color_style.concat([
    	{
    		selector: 'node',
    		style: {
    			label: 'data(id)',
    			'text-opacity': 0.5
    		}
    	},
        {
            selector: 'node.highlight',
            style: {
                label: 'data(id)'
            }
        },
        {
            selector: 'node.transparent',
            style: {
                //shape: 'ellipse',
                'background-color': 'gray',
                label: '',
                opacity: 0.5
            }
        },
        {
        	selector: 'node',
        	style: {
        		width: function(node){return node.length * 10 * Math.pow(Math.log(1+node.degree(false)), 2) / Math.pow(Math.log(1+max_degree), 2)},
        		height: function(node){return node.length * 10 * Math.pow(Math.log(1+node.degree(false)), 2) / Math.pow(Math.log(1+max_degree), 2)}
        	}
        },
        {
        	selector: 'edge',
        	style: {
        		lineColor: 'data(color)',
        		opacity: 0.2
        	}
        },
        {
        	selector: 'edge.highlight',
        	style: {
        		opacity: 1
        	}
        },
        {
        	selector: 'edge.transparent',
        	style: {
        		opacity: 0.01
        	}
        }]),
        layout: {
        	name: whichlayout
        }      
	});

	function clear_all_highlight() {
		cy.elements().removeClass('transparent').removeClass('highlight');
		document.getElementById("cor_list").innerHTML = cyto_prompt;
		document.getElementById("cor_list").style["font-size"] = labelsize + "px";
	}

	function node_highlight(evt) {
		clear_all_highlight();
		// Adapted from Ref 1
		var node = evt.target;
  		cy.elements().difference(node.neighborhood()).not(node).addClass('transparent');
  		node.addClass('highlight').neighborhood().addClass('highlight');
  		let cor_list = "Neighbors of " + node.data("id") + " and the " + (two ? "differential " : "") + "correlations:<br/><br/>";
  		let cor_object = [];
  		for (neighbor_node of node.neighborhood("node"))
  			cor_object.push([neighbor_node.data('id'), node.edgesWith(neighbor_node).data('value')[""]]);
  		cor_object.sort(function(a, b) {return Math.abs(b[1]) - Math.abs(a[1]);});
  		for (let pair of cor_object)
  			cor_list = cor_list + pair[0] + ": " + (Math.round(pair[1]*1000)/1000) + "<br/>";
  		document.getElementById("cor_list").style["font-size"] = cyto_cor_text_size + "px";
  		document.getElementById("cor_list").innerHTML = cor_list;
	}

	function edge_highlight(evt) {
		clear_all_highlight();
		var edge = evt.target;
  		cy.elements().difference(edge.connectedNodes()).not(edge).addClass('transparent');
  		edge.addClass('highlight').connectedNodes().addClass('highlight');
  		console.log(edge);
  		let cor_list = "Node 1: " + edge.data("source") + "<br/><br/>Node 2: " + edge.data("target") + "<br><br/>" + (two ? "Differential c" : "C") + "orrelation: " + (Math.round(edge.data('value')[""]*1000)/1000);
  		document.getElementById("cor_list").style["font-size"] = cyto_cor_text_size + "px";
  		document.getElementById("cor_list").innerHTML = cor_list;
	}

	cy.on('mousedown', (evt) => {
		if (evt.target === cy) clear_all_highlight();
		else if (evt.target.isNode()) node_highlight(evt);
		else if (evt.target.isEdge()) edge_highlight(evt);
	})

	cy.on('cxttap', (evt) => {clear_all_highlight()});


} // Definition of drawCyto