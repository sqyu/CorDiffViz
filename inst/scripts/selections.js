function toggle_div() {
	var d3_div = document.getElementById("D3_div"); 
	var cy_div = document.getElementById("CY_div");
	if (d3_div.style.display !== "none") {
		// Set the left and top positions of CY to those of D3
		// The left margin of cy_right_div will be set afterwards in drawCyto after cy_left is plotted
		var cy_left_div = document.getElementById("cy_left"),
			cy_right_div = document.getElementById("cy_right"),
			d3_svg_rect = d3_div.getBoundingClientRect();
		cy_left_div.style.top = cy_right_div.style.top = cy_div.style.top = d3_svg_rect.top + "px";
		cy_left_div.style.left = cy_div.style.left = d3_svg_rect.left + "px";
		d3_div.style.display = "none"; 
		cy_div.style.display = "inline"; 
	} else {
		d3_div.style.display = "inline"; 
		cy_div.style.display = "none"; 
	}
}

function Draw(only_alpha_changed = false) {
	console.log(whichlayout);
	var datfile = datafilename(cortype, testtype, two, whichdata);
	if (testtype == "raw" || (two && cortype === "pearson" && testtype === "cai")) { // Does not depend on significance level, no need to threshold or replot on change of alpha if variables not changed
		if (only_alpha_changed && !varselected_changed) // If only alpha changed, no need to replot
			return;
		var total_nonzero = 0;
		if (typeof window["prop_" + datfile] === "undefined" || varselected_changed) { // Calculate the proportion of non-zero entries for the first time
			var total_nonzero = 0, corrmat = window[datfile + "_mat"];
			console.log(corrmat);
			if (is_X_Y) { // Sum up and divide
				for (var i = 0; i < corrmat.length; i++)
					total_nonzero += (corrmat[i].value != 0);
				window["prop_" + datfile] = total_nonzero * 100.0 / corrmat.length;
			} else { // Ignore diagonal entries
				for (var i = 0; i < nvar_X - 1; i++)
        			for (var j = i + 1; j < nvar_X; j++)
	           			total_nonzero += (corrmat[j*(j+1)/2+i].value != 0);
	           	window["prop_" + datfile] = total_nonzero * 200.0 / (nvar_X * (nvar_X - 1));
			}
		}
	} else if (typeof window[datfile + "_alpha"] === 'undefined' || window[datfile + "_alpha"] != alpha || varselected_changed) { // Re-threshold only if variables changed or alpha has changed since last thresholding for this matrix
			console.log("Thresholding for " + datfile + " with alpha = " + alpha);
			var raw_mat = window[datafilename(cortype, "raw", two, whichdata) + "_mat"], 
				ps = window[datfile + "_p_mat"],
				total_nonzero = 0;
			window[datfile + "_mat"] = [];
			for (var i = 0; i < raw_mat.length; ++i) {
				var this_significant = (ps[i].value <= alpha);
				total_nonzero += this_significant;
				window[datfile + "_mat"].push({row: ps[i].row, col: ps[i].col, value: (this_significant ? raw_mat[i].value : 0)}); // json written by column in R; so need to reverse when reading in
			}
			window["prop_" + datfile] = total_nonzero * (is_X_Y ? (100.0 / raw_mat.length) : (200.0 / (nvar_X * (nvar_X - 1))));
			window[datfile + "_alpha"] = alpha;
	}
	console.log(window["prop_" + datfile] + "% nonzero");
	if (d3ORctyo) {
		console.log("Plotting Cytoscape for " + datfile)
		drawCyto(cortype, testtype, two, datfile, whichdata, whichlayout);
	}
	else {
		console.log("Plotting D3 for " + datfile)
		drawD3(cortype, testtype, two, datfile, whichdata);
	}
}

function getradiovalue(radios) {
	for (var i = 0; i < radios.length; i++)
		if (radios[i].checked) 
			return (radios[i].value)
}

function datafilename(cortype,testtype,two,whichdata="") {
	return (two?"diff":"cor") + "_" + cortype + "_" + testtype + (two?"":"_"+whichdata)
}

function autohideradios() {
	dataradios.style.display = two ? 'none' : 'inline';
	layoutradios.style.display = d3ORctyo ? 'inline' : 'none';
	testcairadio.style.display = testcaitext.style.display = (two && cortype === "pearson") ? 'inline' : 'none';
	testrawradio.style.display = testrawtext.style.display = (d3ORctyo) ? 'none' : 'inline';
}

function toperm(old_type) {
	testtype = "perm";
	document.getElementById('testtype' + old_type).checked = false;
	document.getElementById('testtypeperm').checked = true;
}