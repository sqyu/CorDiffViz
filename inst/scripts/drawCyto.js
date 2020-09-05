function drawCyto(cortype, testtype, two, datfile, whichrawdat, whichlayout){
	// References:
	// 1: https://stackoverflow.com/questions/31510992/how-to-highlight-neighbouring-nodes-in-cytoscape-js
	// 2: https://stackoverflow.com/questions/49156044/highlighting-edges-with-cytoscape-js-doesnt-work
	scatprompt.selectAll('g').remove();
	corrplot.selectAll('g').remove();
    d3.selectAll('#activated_cell').remove(); // Clicked-on cell in the correlation matrix
	scatterplot1.selectAll('g').remove();
	scatterplot2.selectAll('g').remove();
    console.log(vars_X);

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
                'background-color': 'orange',
            }
        },
        {
            selector: '.Y.highlight',
            style: {
                'background-color': 'green',
            }
        }
		]
	} else {
		node_color_style = [{
			selector: 'node',
			style: {
				'background-color': 'chocolate'
			}
		},
		{
            selector: 'node.highlight',
            style: {
                'background-color': 'chocolate',
            }
        }]
	}
  
	var num_nodes = nvar_X + (is_X_Y ? nvar_Y : 0);
    corrmat = window[datfile + "_mat"];

    var cy = cytoscape({
  		container: document.getElementById('cy_left'),
  		//elements: graph_nodes,
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
                label: 'data(id)',
                'font-size': 30
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
        		width: 1, // Changed later after max degree is determined
        		height: 1 // Changed later after max degree is determined
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
        }])   
	});

    for (var i = 0; i < nvar_X; i++)
        cy.add([{"data": {id: vars_X[i]}, "classes": "X", "group": "nodes", "removed": false, 
            "selected": false, "selectable": true, "locked": false, "grabbed": false, "grabble": true}])
    if (is_X_Y)
        for (var i = 0; i < nvar_Y; i++)
            cy.add([{"data": {id: vars_Y[i]}, "classes": "Y", "group": "nodes", "removed": false, 
                "selected": false, "selectable": true, "locked": false, "grabbed": false, "grabble": true}])

    for (var row = 0; row < (is_X_Y ? nvar_X : (nvar_X - 1)); row++)
        for (var col = (is_X_Y ? 0 : (row + 1)); col < nvar_Y; col++) {
            var this_val = corrmat[get_index(col, row, nvar_X, is_X_Y)].value;
            if (this_val !== 0)
                cy.add([{
                    "data": {
                        id: "edge_" + row + "_" + col,
                        source: vars_X[row],
                        target: vars_Y[col],
                        value: corrmat[get_index(col, row, nvar_X, is_X_Y)].value,
                        color: corColScale(this_val),
                    },
                    "group": "edges", "removed": false, "selected": false, "selectable": true, "locked": false,
                    "grabbed": false, "grabble": true, "classes": ""
                }]);
        }

    console.log(cy.nodes().degree(false));

    cy.ready(function() {
        max_degree = cy.nodes().reduce(function(max_sofar, node) {return Math.max(max_sofar, node.degree(false));}, 0),
        console.log("Max node degree = " + max_degree);
        node_size_mult_min = 500 / num_nodes, node_size_mult_max = 2 * node_size_mult_min,
        node_size_slope = (node_size_mult_max - node_size_mult_min) / Math.sqrt(Math.max(1, max_degree)),
        node_size_func = function(node){return node_size_mult_min + Math.sqrt(node.degree(false)) * node_size_slope};
        cy.nodes().style("width", node_size_func);
        cy.nodes().style("height", node_size_func);

        /* Attempt to resize
        var cy_left_elt = document.getElementById("cy_left"),
        cy_right_elt = document.getElementById("cy_right");
        cy_left_elt.setAttribute("width", cy_left_width * plot_scale);
        cy_left_elt.setAttribute("height", cy_height * plot_scale);
        if (cy_left_elt.childNodes.length) {
            cy_left_elt.childNodes[0].style.width = (cy_left_width * plot_scale) + "px";
            cy_left_elt.childNodes[0].style.height = (cy_height * plot_scale) + "px";
            console.log(cy_left_elt.childNodes[0]);
            console.log(cy_left_elt.childNodes[0].style);
        }
        cy_right_elt.setAttribute("width", cy_right_width * plot_scale);
        cy_right_elt.setAttribute("height", cy_height * plot_scale);
        cy_right_elt.style.left = (document.getElementById("cy_left").getBoundingClientRect().right + 10) + "px"; // Change the left position of cor_list
            
        var canvases = document.querySelectorAll('canvas');
        if (canvases)
            canvases.forEach(function(canvas){
                // Make it visually fill the positioned parent
                canvas.style.width ='100%';
                canvas.style.height='100%';
                // ...then set the internal size to match
                canvas.width  = canvas.offsetWidth;
                canvas.height = canvas.offsetHeight;
            });
        console.log(canvases);

        cy.resize();
        cy.fit();*/

        cy.elements().layout({"name": whichlayout}).run();
        
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
  			cor_object.push([neighbor_node.data('id'), node.edgesWith(neighbor_node).data('value')]);
  		cor_object.sort(function(a, b) {return Math.abs(b[1]) - Math.abs(a[1]);});
  		for (let pair of cor_object)
  			cor_list = cor_list + pair[0] + ": " + rounding(pair[1], 3) + "<br/>";
  		document.getElementById("cor_list").style["font-size"] = cyto_cor_text_size + "px";
  		document.getElementById("cor_list").innerHTML = cor_list;
	}

	function edge_highlight(evt) {
		clear_all_highlight();
		var edge = evt.target;
  		cy.elements().difference(edge.connectedNodes()).not(edge).addClass('transparent');
  		edge.addClass('highlight').connectedNodes().addClass('highlight');
  		console.log(edge);
  		let cor_list = "Node 1: " + edge.data("source") + "<br/><br/>Node 2: " + edge.data("target") + "<br><br/>" + (two ? "Differential c" : "C") + "orrelation: " + rounding(edge.data('value'), 3);
  		document.getElementById("cor_list").style["font-size"] = cyto_cor_text_size+ "px";
  		document.getElementById("cor_list").innerHTML = cor_list;
	}

	cy.on('mousedown', (evt) => {
		if (evt.target === cy) clear_all_highlight();
		else if (evt.target.isNode()) node_highlight(evt);
		else if (evt.target.isEdge()) edge_highlight(evt);
	})

	cy.on('cxttap', (evt) => {clear_all_highlight()});


} // Definition of drawCyto