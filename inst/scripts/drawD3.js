
// Attention: X is the variable defined by the rows and Y is the variable defined by the columns, opposite to x and y in Javascript.
svg = d3.select('#D3_div').append('svg')
	.attrs({
		'class': 'svg',
		'id': 'svg'
	});
svg_g = svg.append('g')
	.attr('id', 'svg_g');
corrplot = svg_g.append('g')
	.attr('id', 'corrplot'),
scatprompt = svg_g.append('g')
	.attr('id', 'scatprompt');
scatterplot1 = svg_g.append('g')
	.attr('id', 'scatterplot1');
scatterplot2 = svg_g.append('g')
	.attr('id', 'scatterplot2');

function get_index(col, row, nvar_X, is_X_Y) {
	if (is_X_Y) // X_Y case 
		return (col * nvar_X + row);
	if (row > col) {let tmp = row; row = col; col = tmp;} // X case: col must be >= row
	return (col*(col+1)/2 + row); // upper triangle
}

var drawScatter = function(col, row, whichrawdat, flipped) {
	// flipped indicates if the scatter plot plots X[row] vs Y[col]/X[col] (false) or Y[col]/X[col] vs X[row] (true)
	console.log('Showing column ' + col + ', row ' + row + (flipped ? ", flipped" : ""));

	d3.selectAll('.points').remove();
	d3.selectAll('.axis').remove();
	d3.selectAll('.scatterlabel').remove();
	d3.selectAll('.scatplottitle1').remove();
	d3.selectAll('.scatplottitle2').remove(); 
	scatprompt.selectAll('g').remove();

	var Y_extent1 = d3.extent((is_X_Y ? dat1_Y : dat1)[col]),
		X_extent1 = d3.extent((is_X_Y ? dat1_X : dat1)[row]);

	var xScale1 = d3.scaleLinear()
		.domain(flipped ? X_extent1 : Y_extent1) // Recall: x is the col coordinate, but X is the data indexed by rows
		.range([0, w_scat * plot_scale]);
	var yScale1 = d3.scaleLinear()
		.domain(flipped ? Y_extent1 : X_extent1)
		.range([scatterplot_height, 0]);

	var xAxis1 = d3.axisBottom(xScale1)
		.ticks(num_ticks)

	var yAxis1 = d3.axisLeft(yScale1)
		.ticks(num_ticks)

	if (two){
		var Y_extent2 = d3.extent((is_X_Y ? dat2_Y : dat2)[col]),
			X_extent2 = d3.extent((is_X_Y ? dat2_X : dat2)[row]);
		var xScale2 = d3.scaleLinear()
			.domain(flipped ? X_extent2 : Y_extent2)
			.range([0, w_scat * plot_scale]);
		var yScale2 = d3.scaleLinear()
			.domain(flipped ? Y_extent2 : X_extent2)
			.range([scatterplot_height, 0]);
		var xAxis2 = d3.axisBottom(xScale2)
			.ticks(num_ticks);

		var yAxis2 = d3.axisLeft(yScale2)
			.ticks(num_ticks);
	}

	var plot1_name = window[whichrawdat+"_name"],
		raw_cor1 = ((!is_X_Y) && col === row) ? 1 : rounding(window["cor_"+cortype+"_raw_"+whichrawdat+"_mat"][get_index(col, row, nvar_X, is_X_Y)].value, 3);

	var plot_scatter_title = function(whichdat, title = "") { 
	// Plot 1 if whichdat is true, plot 2 otherwise
		if (title === "") {
			title = 'Scatter plot, ' + (whichdat ? plot1_name : plot2_name) + ', raw ' + (whichdat ? raw_cor1 : raw_cor2);
			title_class = whichdat ? 'scatplottitle1' : 'scatplottitle2';
			this_label_size = labelsize;
		} else {
			title_class = "scatplottitle_tmp";
			this_label_size = labelsize - 2;
		}
		scplt_obj = whichdat ? scatterplot1 : scatterplot2;
		scplt_obj.append('g')
		.append('text')
		.text(title)
		.attrs({
			'class': title_class,
			'x': (w_scat/2) * plot_scale,
			'y': -labelsize * plot_scale,
			'dominant-baseline': 'middle',
			'text-anchor': 'middle'
		})
		.style("font-size", this_label_size * plot_scale+"px");
	}

	plot_scatter_title(true);

	if (two){
		var plot2_name = second_name,
			raw_cor2 = ((!is_X_Y) && col === row) ? 1 : rounding(window["cor_"+cortype+"_raw_second_mat"][get_index(col, row, nvar_X, is_X_Y)].value, 3);

		plot_scatter_title(false);
	}

	var X_name = vars_X[row];
		Y_name = vars_Y[col];
	if (flipped) {
		var cx_dat1 = function(d) {return (is_X_Y ? dat1_X : dat1)[row][d];}
		var cy_dat1 = function(d) {return (is_X_Y ? dat1_Y : dat1)[col][d];};
	} else {
		var cx_dat1 = function(d) {return (is_X_Y ? dat1_Y : dat1)[col][d];}
		var cy_dat1 = function(d) {return (is_X_Y ? dat1_X : dat1)[row][d];};
	}
	var cx_func1 = function(d) {return xScale1(cx_dat1(d));},
		cy_func1 = function(d) {return yScale1(cy_dat1(d));};

	function point_mouseover(d, whichdat) { // Plot 1 if whichdat, else plot 2
		if (whichdat) {	
			var scplt_obj = scatterplot1,
				cx_func = cx_func1;
				cy_func = cy_func1;
		} else {
			var scplt_obj = scatterplot2,
				cx_func = cx_func2;
				cy_func = cy_func2;
		}
		scplt_obj.append('g')
		.append('line')
		.attrs({
			"id": 'line_to_axes',
			"stroke-dasharray": "1 4",
			"x1": 0,
			"y1": cy_func(d),
			"x2": cx_func(d),
			"y2": cy_func(d),
			"stroke-width": 2,
			"stroke": "black"
		});
		scplt_obj.append('g')
		.append('line')
		.attrs({
			"id": 'line_to_axes',
			"stroke-dasharray": "1 4",
			"x1": cx_func(d),
			"y1": scatterplot_height,
			"x2": cx_func(d),
			"y2": cy_func(d),
			"stroke-width": 2,
			"stroke": "black"
		});
		if (whichdat) {
			d3.selectAll('.scatplottitle1').remove();
			// If is_X_Y, dat1_X and dat1_Y have the same indices so does not matter
			plot_scatter_title(true, "ID " + cut_string(ind_first[d], 8) + ", " + plot1_name + ", " + "(" + rounding(cx_dat1(d), 3) + ", " + rounding(cy_dat1(d), 3) + ")");
		} else {
			d3.selectAll('.scatplottitle2').remove();
			plot_scatter_title(false, "ID " + cut_string(ind_second[d], 8) + ", " + plot2_name + ", " + "(" + rounding(cx_dat2(d), 3) + ", " + rounding(cy_dat2(d), 3) + ")");
		}
	}

	scatterplot1.append('g')
	.attr('class', 'points')
	.selectAll('empty')
	.data(d3.range(nind1))
	.enter().append('circle')
	.attr('data-name', function (d) {'scatter_point' + d;})
	.attrs({
		'class': 'point',
		'cx': cx_func1,
		'cy': cy_func1,
		'r': 5 * plot_scale,
		'stroke': 'none',
		'fill': 'black'
	})
	.on("mouseover", function(d) {point_mouseover(d, true);}
	)
	.on("mouseout", function(d) {
		d3.selectAll('#line_to_axes').remove();
		d3.selectAll('.scatplottitle_tmp').remove();
		plot_scatter_title(true);
	});

	console.log("Showing scatter plot of " + plot1_name)

	scatterplot1.append('g')
	.attr('class', 'x axis')
	.attr('transform', 'translate(' + 0 + ',' + scatterplot_height + ')')
	.call(xAxis1)
	.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});

	scatterplot1.append('g')
	.attr('class', 'y axis')
	.attr('transform', 'translate(' + 0 + ', 0)')
	.call(yAxis1)
	.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});

	scatterplot1.append('g').append('text') // variable name for x axis
	.text(flipped ? X_name : Y_name)
	.attrs({
		'class': 'scatterlabel',
		'x': w_scat * plot_scale / 2,
		'y': (h + tickandaxis) * plot_scale,
		'text-anchor': 'middle',
		'dominant-baseline': 'middle'
	})
	.style("font-size", labelsize * plot_scale+"px")
	.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});

	scatterplot1.append('g').append('text') // variable name for y axis
	.text(flipped ? Y_name : X_name) 
	.attrs({
		'class': 'scatterlabel',
		'transform': 'translate(' + (w_scat+labelsize) * plot_scale + ',' + (h/2) * plot_scale + ')rotate(270)',
		'dominant-baseline': 'middle',
		'text-anchor': 'middle'
	})
	.style("font-size", labelsize * plot_scale+"px")
	.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});

	if (two){
		if (flipped) {
			var cx_dat2 = function(d) {return (is_X_Y ? dat2_X : dat2)[row][d];}
			var cy_dat2 = function(d) {return (is_X_Y ? dat2_Y : dat2)[col][d];};
		} else {
			var cx_dat2 = function(d) {return (is_X_Y ? dat2_Y : dat2)[col][d];}
			var cy_dat2 = function(d) {return (is_X_Y ? dat2_X : dat2)[row][d];};
		}
		var cx_func2 = function(d) {return xScale2(cx_dat2(d));},
			cy_func2 = function(d) {return yScale2(cy_dat2(d));};


		scatterplot2.append('g')
		.attr('class', 'points')
		.selectAll('empty')
		.data(d3.range(nind2))
		.enter().append('circle')
		.attrs({
			'class': 'point',
			'cx': cx_func2,
			'cy': cy_func2,
			'r': 5 * plot_scale,
			'stroke': 'none',
			'fill': 'black'
		})
		.on("mouseover", function(d) {point_mouseover(d, false);}
		)
		.on("mouseout", function(d) {
			d3.selectAll('#line_to_axes').remove();
			d3.selectAll('.scatplottitle_tmp').remove();
			plot_scatter_title(false);
		});
		
		console.log("Showing scatter plot of " + plot2_name)
		scatterplot2.append('g')
		.attr('class', 'x axis')
		.attr('transform', 'translate(' + 0 + ',' + scatterplot_height  + ')')
		.call(xAxis2)
		.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});

		scatterplot2.append('g')
		.attr('class', 'y axis')
		.attr('transform', 'translate(' + 0 + ', 0)')
		.call(yAxis2)
		.on("click", function() {drawScatter(col, row, whichrawdat, !flipped);});
	}


	svg.selectAll(".x.axis>.tick>text,.y.axis>.tick>text")
	.each(function(d, i){d3.select(this).style("font-size", ticksize * plot_scale + "px");});


}; // Definition of drawScatter


function drawD3(cortype, testtype, two, datfile, whichrawdat="first"){
	click_activated = false;
	corrplot.selectAll('g').remove();
	scatprompt.selectAll('g').remove();
	scatterplot1.selectAll('g').remove();
	scatterplot2.selectAll('g').remove();
	d3.selectAll('#activated_cell').remove();
	console.log("D3 Showing "+datfile);

	scatterplot_height = (two ? (h-h_btw_scat-labelsize)/2 - tickandaxis : h-tickandaxis) * plot_scale;

	var svg_elt = document.getElementById("svg"),
		svg_g_elt = document.getElementById("svg_g"),
		corrplot_elt = document.getElementById("corrplot"),
		scatprompt_elt = document.getElementById("scatprompt"),
		scatterplot1_elt = document.getElementById("scatterplot1"),
		scatterplot2_elt = document.getElementById("scatterplot2");
	svg_elt.setAttribute("width", (full_width + margin.left + margin.right) * plot_scale);
	svg_elt.setAttribute("height", (h + margin.top + margin.bottom + 2 * labelsize) * plot_scale);
	svg_g_elt.setAttribute("transform", 'translate(' + margin.left * plot_scale + ',' + margin.top * plot_scale + ')');
	svg_g_elt.setAttribute("width", full_width * plot_scale);
	svg_g_elt.setAttribute("height", h * plot_scale);
	corrplot_elt.setAttribute("transform", 'translate(' + w_smaller * plot_scale / 2 + ',' + h_smaller * plot_scale / 2 + ')');
	corrplot_elt.setAttribute("width", (w - w_smaller) * plot_scale);
	corrplot_elt.setAttribute("height", (h - h_smaller) * plot_scale);
	scatprompt_elt.setAttribute("transform", 'translate(' + (w + pad) * plot_scale + ',' + h * plot_scale / 2 + ')');
	scatterplot1_elt.setAttribute("transform", 'translate(' + (w + pad) * plot_scale + ',0)')
	scatterplot1_elt.setAttribute("width", w_scat * plot_scale);
	scatterplot2_elt.setAttribute("transform", 'translate(' + (w + pad) * plot_scale + ',' + (scatterplot_height + (tickandaxis + h_btw_scat + labelsize) * plot_scale) + ')') // Starts right after second title
	scatterplot2_elt.setAttribute("width", w_scat * plot_scale);

	corrplot.append('g').append('rect')
		.attrs({
			'class': 'corrbox',
			'x': 0, 'y': 0,
			'width': (w - w_smaller) * plot_scale,
			'height': (h - h_smaller) * plot_scale,
			'fill': 'none',
			'stroke': 'black',
			'stroke-dasharray': '20,20',
			'stroke-width': '1'
		});

	if (two){
		if (is_X_Y) {
			dat1_X = dat_first_X;
			dat1_Y = dat_first_Y;
			dat2_X = dat_second_X;
			dat2_Y = dat_second_Y;
		} else {
			dat1 = dat_first;
			dat2 = dat_second;
		}
		nind1 = nind_first;
		nind2 = nind_second;
		whichrawdat="first";
	} else {
		if (is_X_Y) {
			dat1_X = window["dat_" + whichrawdat + "_X"];
			dat1_Y = window["dat_" + whichrawdat + "_Y"];
		} else {
			dat1 = window["dat_" + whichrawdat]
		}
		nind1 = window["nind_" + whichrawdat];
	}
	corrmat = window[datfile + "_mat"];

	scatprompt.append('g').append('text')
	.attr('dy', '-2em')
	.text('Click on an entry of the')
	.style("font-size", labelsize * plot_scale+"px");
	scatprompt.append('g').append('text')
	.attr('dy', '0em')
	.text('correlation matrix on the left')
	.style("font-size", labelsize * plot_scale+"px");
	scatprompt.append('g').append('text')
	.attr('dy', '2em')
	.text('to explore the scatter plot(s).')
	.style("font-size", labelsize * plot_scale+"px");

	corrplot.append('g').append('text')
	.text((two ? 'Differential correlation matrix' : 'Correlation matrix') + ": " + rounding(window["prop_"+datfile], 2) + "% nonzero")
	.attrs({
		'class': 'corrplottitle',
		'x': (w - w_smaller) * plot_scale / 2,
		'y': -(margin.top + h_smaller) * plot_scale / 2,
		'dominant-baseline': 'middle',
		'text-anchor': 'middle'
	})
	.style("font-size", labelsize * plot_scale +"px");

	var corXscale = d3.scaleBand().rangeRound([0, (w - w_smaller) * plot_scale]).domain(d3.range(nvar_Y)),
		corYscale = d3.scaleBand().rangeRound([0, (h - h_smaller) * plot_scale]).domain(d3.range(nvar_X)),
		corRscale = d3.scaleSqrt().range([0, 0.5*corXscale.bandwidth()]).domain([0,1]); ////// corXscale.bandwidth() == corYscale.bandwidth() anyways


	var cells = corrplot.append('g')
		.attr('id', 'cells')
		.selectAll('empty')
		.data(corrmat)
		.enter().append('g')
		.attrs({
			'class': 'cell'
		})
		.style('pointer-events', 'all');

	console.log(corrmat)

	var rects = cells.append('rect')
		.attrs({
			'x': function(d) { return corXscale(d.col); },
			'y': function(d) { return corYscale(d.row); },
			'width': corXscale.bandwidth(),
			'height': corYscale.bandwidth(),
			'fill': 'none',
			'stroke': '#ccc',
			'stroke-dasharray': '1 4',
			'stroke-width': '1'
		});

	var circles = cells.append('circle')
		.attr('cx', function(d) {return corXscale(d.col) + 0.5*corXscale.bandwidth(); })
		.attr('cy', function(d) {return corYscale(d.row) + 0.5*corYscale.bandwidth(); })
		.attr('r', function(d) {return corRscale(Math.abs(d.value)); })
		.style('fill', function(d) { return corColScale(d.value); }); // Defined in preplot.js

	function activate_cell(this_obj, xPos, yPos, col, row, value) {
		d3.select(this_obj)
		.select('rect')
		.attr('stroke', 'black')
		.attr('stroke-dasharray', 'none');

		corrplot.append('text')
		.attrs({
			'id': "activated_cell",
			'class': 'corrlabel',
			'x': corXscale(col) + 0.5*corXscale.bandwidth(),
			'y': (h - h_smaller + labelsize) * plot_scale
		})
		.text(vars_Y[col])
		.attrs({
			'dominant-baseline': 'middle',
			'text-anchor': 'middle'
		})
		.style("font-size", labelsize * plot_scale+"px");

		corrplot.append('text')
		.attrs({
			'id': "activated_cell",
			'class': 'corrlabel'
			// 'x': -margin.left*0.1,
			// 'y': corXscale(row)
		})
		.text(vars_X[row])
		.attrs({
			'dominant-baseline': 'middle',
			'text-anchor': 'middle',
			'transform': 'translate(' + (-labelsize) * plot_scale + ',' + (corYscale(row) + 0.5*corYscale.bandwidth()) + ')rotate(270)'
		})
		.style("font-size", labelsize * plot_scale+"px");

		corrplot.append('rect')
		.attrs({
			'id': "activated_cell",
			'class': 'tooltip',
			'x': xPos - 20 * plot_scale + corXscale.bandwidth() / 2,
			'y': yPos - 30 * plot_scale,
			'width': 40 * plot_scale,
			'height': 20 * plot_scale,
			'fill': 'rgba(200, 200, 200, 0.5)',
			'stroke': 'black'
		});

		corrplot.append('text')
		.attrs({
			'id': "activated_cell",
			'class': 'tooltip',
			'x': xPos + corXscale.bandwidth() / 2,
			'y': yPos - 15 * plot_scale,
			'text-anchor': 'middle',
			'font-family': 'sans-serif',
			'font-size': 14 * plot_scale + 'px',
			'font-weight': 'bold',
			'fill': 'black'
		})
		.text(d3.format('.2f')(value));
	}

	corrplot.selectAll('g.cell')
	.on('click mouseover', function(d) {
		d3.selectAll('#activated_cell').remove();

		var xPos = parseFloat(d3.select(this).select('rect').attr('x'));
		var yPos = parseFloat(d3.select(this).select('rect').attr('y'));
		if (click_activated && xPos === clicked_corr_xPos && yPos === clicked_corr_yPos)
			return;

		if (d3.event.type === "click") {
			if (click_activated)
				d3.select(clicked_corr_this)
				.select('rect')
				.attr('stroke', '#ccc')
				.attr('stroke-dasharray', '1 4')
				.attr('stroke-width', '1'); // Remove the black square for the previously clicked cell
			clicked_corr_this = this;
			clicked_corr_xPos = xPos;
			clicked_corr_yPos = yPos;
			click_activated = true;
			clicked_corr_col = d.col;
			clicked_corr_row = d.row;
			clicked_corr_value = d.value;
		}

		activate_cell(this, xPos, yPos, d.col, d.row, d.value);

		if (d3.event.type === "click")
			drawScatter(d.col, d.row, whichrawdat, false);
	}) // function for mouseover
	.on('mouseout', function(d) {
		d3.selectAll('#activated_cell').remove();
		//d3.selectAll('.corrlabel').remove();
		d3.select(this)
			.select('rect')
			.attr('stroke', '#ccc')
			.attr('stroke-dasharray', '1 4')
			.attr('stroke-width', '1');

		if (click_activated)
			activate_cell(clicked_corr_this, clicked_corr_xPos, clicked_corr_yPos, 
				clicked_corr_col, clicked_corr_row, clicked_corr_value);

		//Hide the tooltip
		//d3.selectAll('.tooltip').remove();

	})


} // Definition of drawD3