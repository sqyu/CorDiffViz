var is_X_Y = (typeof vars_X !== 'undefined');

if (is_X_Y) {
	for (var varname in window){
		if (varname.includes("_") && (varname.substring(0,3) == 'cor' || varname.substring(0,4) == 'diff')){ ////
			window[varname + "_mat"] = [];
			for (var i = 0; i < window[varname].length; ++i) {
				for (var j = 0; j < window[varname][i].length; ++j) {
					window[varname+"_mat"].push({row: j, col: i, value:window[varname][i][j]}); // json written by column in R; so need to reverse when reading in
				}
			}
			window[varname] = null;
		}
	}
	var nind_first = dat_first_X.ind.length,
		nind_second = dat_second_X.ind.length,
		nvar_X = vars_X.length,
		nvar_Y = vars_Y.length;

	for (let i = 0; i < nvar_X; i++)
		vars_X[i] = cut_string(vars_X[i], 15);
	for (let i = 0; i < nvar_Y; i++)
		vars_Y[i] = cut_string(vars_Y[i], 15);

} else {
	for (var varname in window){
		if (varname.includes("_") && (varname.substring(0,3) == 'cor' || varname.substring(0,4) == 'diff')){ ////
			window[varname + "_mat"] = [];
			for (var i = 0; i < window[varname].length; ++i) {
				for (var j = 0; j <= i; ++j) {
					window[varname+"_mat"].push({row: j, col: i, value:window[varname][i][j]}); // json written by column in R; so need to reverse when reading in
				}
			}
			window[varname] = null;
		}
	}

	var nind_first = dat_first.ind.length,
		nind_second = dat_second.ind.length,
		nvar_X = vars.length,
		nvar_Y = vars.length,
		vars_X = vars_Y = vars;

	for (let i = 0; i < nvar_X; i++)
		vars[i] = cut_string(vars[i], 15);
}

if (nvar_Y > nvar_X) { // more variables to plot on the x axis, make h smaller and fill the margin
	h_smaller = h - h / nvar_Y * nvar_X;
	w_smaller = 0;
} else {
	w_smaller = w - w / nvar_X * nvar_Y;
	h_smaller = 0;
}


function cut_string(s, len) {
	if (s.length > len)
		return s.substring(0, len) + "..."
	return s
}

first_name = cut_string(first_name, 6);
second_name = cut_string(second_name, 6);

document.getElementById("whichdatalabelFIRST").innerHTML = first_name
document.getElementById("whichdatalabelSECOND").innerHTML = second_name


var cyto_cor_text_size = Math.min(labelsize, 15);
var cyto_prompt = "Please left click on/drag a node or left click on an edge on the left.</br></br>You may also zoom in/out the canvas.</br></br>Right click, or left click on the white spaces to the left/right of the graph to reset.";
document.getElementById("cor_list").style["font-size"] = labelsize + "px";
document.getElementById("cor_list").innerHTML = cyto_prompt;

corColScale = d3.scaleLinear().domain([-1,0,1]).range(['crimson','white','slateblue']);


