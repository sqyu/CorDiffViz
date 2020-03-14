var is_X_Y = (typeof vars_X !== 'undefined');



if (is_X_Y) {
	for (var varname in window){
	if (varname.includes("_") && (varname.substring(0,3) == 'cor' || varname.substring(0,4) == 'diff')){
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
		nvar_Y = vars_Y.length
} else {
	for (var varname in window){
	if (varname.includes("_") && (varname.substring(0,3) == 'cor' || varname.substring(0,4) == 'diff')){
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
		nvar_Y = vars.length
}

if (nvar_Y > nvar_X) { // more variables to plot on the x axis, make h smaller and fill the margin
	h_smaller = h - h / nvar_Y * nvar_X;
	w_smaller = 0;
} else {
	w_smaller = w - w / nvar_X * nvar_Y;
	h_smaller = 0;
}

document.getElementById("whichdatalabelFIRST").innerHTML = first_name
document.getElementById("whichdatalabelSECOND").innerHTML = second_name


var cyto_cor_text_size = Math.min(labelsize, 15);
var cyto_prompt = "Please left click on/drag a node or left click on an edge on the left.</br></br>You may also zoom in/out the canvas.</br></br>Right click, or left click on the white spaces to the left/right of the graph to reset.";
document.getElementById("cor_list").style["font-size"] = labelsize + "px";
document.getElementById("cor_list").innerHTML = cyto_prompt;


