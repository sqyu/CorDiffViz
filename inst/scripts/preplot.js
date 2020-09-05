var is_X_Y = (typeof vars_X !== 'undefined');

if (is_X_Y) {
	var ind_first = dat_first_X.ind,
		ind_second = dat_second_X.ind;
	for (let i = 0; i < nvar_X; i++)
		vars_X[i] = cut_string(vars_X[i], 15);
	for (let i = 0; i < nvar_Y; i++)
		vars_Y[i] = cut_string(vars_Y[i], 15);
} else {
	var ind_first = dat_first.ind,
		ind_second = dat_second.ind,
		vars_X = vars_Y = vars;

	for (let i = 0; i < nvar_X; i++)
		vars[i] = cut_string(vars[i], 15);
}
var nind_first = ind_first.length,
	nind_second = ind_second.length,
	nvar_X = vars_X.length,
	nvar_Y = vars_Y.length;

for (var varname in window){
	/* Vectorize cor and diff matrices and save as orig */
	if (varname.includes("_") && (varname.substring(0,3) == 'cor' || varname.substring(0,4) == 'diff')){ ////
		window[varname + "_mat_orig"] = [];
		for (var i = 0; i < window[varname].length; ++i)
			for (var j = 0, end_at = (is_X_Y ? window[varname][i].length-1 : i); j <= end_at; ++j) 
				window[varname + "_mat_orig"].push({row: j, col: i, value:window[varname][i][j]}); // json written by column in R; so need to reverse when reading in
		window[varname] = null;
	} else if (varname.substring(0,3) == 'dat') {
		/* Save e.g. dat1.dat as dat1_orig */
		window[varname + "_orig"] = [];
		for (var i = 0; i < window[varname].dat.length; ++i) {
			window[varname + "_orig"][i] = [];
			for (var j = 0; j < window[varname].dat[i].length; ++j)
				window[varname + "_orig"][i].push(window[varname].dat[i][j]);
		}
		window[varname] = null;
	}
}

function slice_data_for_vars(selected_bools_X, selected_bools_Y=null) {
	get_shifted_indices = function(bools) {
		shifted_indices = []; // If varselected = [true, false, true, false, false, true], this will be [0, NaN, 1, NaN, NaN, 2]
		var count_true = 0;
		for (var i = 0; i < bools.length; i++)
			shifted_indices[i] = bools[i] ? (count_true++) : NaN;
		return shifted_indices;
	}
	shifted_indices_X = get_shifted_indices(selected_bools_X);
	if (!is_X_Y) {
		selected_bools_Y = selected_bools_X;
		shifted_indices_Y = shifted_indices_X;
	} else shifted_indices_Y = get_shifted_indices(selected_bools_Y);

	for (var varname_orig in window){
		if (varname_orig.endsWith("_orig") && varname_orig !== "vars_X_orig" && varname_orig !== "vars_Y_orig") {
			var varname = varname_orig.replace("_orig", "");
			window[varname] = [];
			if (varname.endsWith("_mat")){ // Vectorized correlations/ differential correlations
				for (var entry of window[varname_orig]) {
					if (selected_bools_X[entry.row] && selected_bools_Y[entry.col])
						window[varname].push({row: shifted_indices_X[entry.row], col: shifted_indices_Y[entry.col], value:entry.value});
				}
				//console.log(varname, window[varname], window[varname_orig]);
			} else if (varname.substring(0,3) == 'dat') { // Data matrix
				if (varname === "dat_first_Y" || varname === "dat_second_Y")
					for (var i = 0; i < window[varname_orig].length; ++i) {
						if (typeof vars_Y_orig !== "undefined")
						if (selected_bools_Y[i]) {
							window[varname][shifted_indices_Y[i]] = [];
							for (var j = 0; j < window[varname_orig][i].length; ++j)
								window[varname][shifted_indices_Y[i]].push(window[varname_orig][i][j]);
						}
					}
				else 
					for (var i = 0; i < window[varname_orig].length; ++i) {
						if (typeof vars_X_orig !== "undefined")
						if (selected_bools_X[i]) {
							window[varname][shifted_indices_X[i]] = [];
							for (var j = 0; j < window[varname_orig][i].length; ++j)
								window[varname][shifted_indices_X[i]].push(window[varname_orig][i][j]);
						}
					}
			}
		}
	}
}


varselected = new Array(nvar_X).fill(true);
if (is_X_Y)
	varselected_Y = new Array(nvar_Y).fill(true);
slice_data_for_vars(varselected, is_X_Y ? varselected_Y : null);

vars_X_orig = [...vars_X];
vars_Y_orig = [...vars_Y];

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


function sort_and_add_options (arr, suffix) {
	var indices = [...Array(arr.length).keys()]; //[0, 1, ..., arr.length]
	indices.sort(function (i, j) { return arr[i].localeCompare(arr[j], 'en', {'sensitivity': 'base'})});
	for (let i = 0; i < arr.length; i++) {
		var newCheckbox = document.createElement('input');
	    newCheckbox.type = "checkbox";
	    newCheckbox.className = "choose_var_box" + suffix;
	    newCheckbox.checked = true;
		newCheckbox.id = arr[indices[i]];
		newCheckbox.value = indices[i];
		newCheckbox.setAttribute("onclick", "varselected" + suffix + "[" + indices[i] + "] = this.checked; varselected_changed = true;")
		var label = document.createElement('label');
		label.htmlFor = arr[indices[i]];
		label.appendChild(newCheckbox);
		label.appendChild(document.createTextNode(arr[indices[i]]));
		document.getElementById("checkboxes").appendChild(label);
	}
}

function add_select_all(suffix) {
	var newCheckbox = document.createElement('input');
    newCheckbox.type = "checkbox";
    newCheckbox.checked = true;
	newCheckbox.id = "selectAll" + suffix;
	newCheckbox.setAttribute("onclick", "toggle_selectall(this, '" + suffix + "');");
	var label = document.createElement('label');
	label.htmlFor = "selectAll" + suffix;
	label.appendChild(newCheckbox);
	label.appendChild(document.createTextNode("Select All"));
	document.getElementById("checkboxes").appendChild(label);
}


if (is_X_Y) {
	var span = document.createElement("div");
	span.appendChild(document.createTextNode("First Group:"));
	span.setAttribute("style", "background-color: white;");
	document.getElementById("checkboxes").appendChild(span);
}
add_select_all("");
sort_and_add_options(vars_X_orig, "");
if (is_X_Y) {
	var span = document.createElement("div");
	span.appendChild(document.createTextNode("Second Group:"));
	span.setAttribute("style", "background-color: white;");
	document.getElementById("checkboxes").appendChild(span);
	add_select_all("_Y");
	sort_and_add_options(vars_Y_orig, "_Y");
}


var expanded = false, varselected_changed = false;
function showCheckboxes() {
  var checkboxes = document.getElementById("checkboxes");
  if (!expanded) {
    checkboxes.style.display = "block";
    expanded = true;
  } else {
	  	// Closed options
	    checkboxes.style.display = "none";
	    expanded = false;
	    // Update variables to plot
	    function get_active_vars(suffix) {
	    	/*checkboxes = document.getElementsByClassName('choose_var_box' + suffix);
	    	var active_vars = [], active_inds = [];
	    	for (var i = 0, n = checkboxes.length; i < n; i++)
	    		active_bools[i] = checkboxes[i].checked;
	    		if (active_bools[i])
		    		active_vars.push(checkboxes[i].id);
	    	return [active_vars, active_bools]*/
	    }
	    if (varselected_changed) {
	    	vars_X = vars_X_orig.filter((v, i) => varselected[i]);
	    	vars_Y = is_X_Y ? (vars_Y_orig.filter((v, i) => varselected_Y[i])) : vars_X;
	    	nvar_X = vars_X.length;
	    	nvar_Y = vars_Y.length;
	    	if (is_X_Y) {
	    		if (nvar_X === 0 || nvar_Y === 0)
	    			alert("You must select at least one variable from each group.");
	    	} else if (nvar_X <= 1)
	    		alert("You must select at least two variables.");	    		
	    	slice_data_for_vars(varselected, is_X_Y ? varselected_Y : varselected);
	    	if (nvar_Y > nvar_X) { // more variables to plot on the x axis, make h smaller and fill the margin
	    		h_smaller = h - h / nvar_Y * nvar_X;
	    		w_smaller = 0;
	    	} else {
	    		w_smaller = w - w / nvar_X * nvar_Y;
	    		h_smaller = 0;
	    	}
	    	Draw(false);
	    	varselected_changed = false;
	    }
/*
	    new_vars_bools_X = get_active_vars("");
	    if (is_X_Y)
	    	new_vars_bools_Y = get_active_vars("_Y");
	    console.log(vars_X, new_vars_bools_X);
	    console.log(vars_Y, new_vars_bools_Y);
	    if (vars_X !== new_vars_bools_X[0] || (is_X_Y && vars_Y !== new_vars_bools_Y[0])) {
	    	vars_X = new_vars_bools_X[0];
	    	nvar_X = vars_X.length;
	    	if (is_X_Y) {
	    		vars_Y = new_vars_bools_Y[0];
	    		nvar_Y = vars_Y.length;
	    	}
	    	slice_data_for_vars(new_vars_bools_X[1], is_X_Y ? new_vars_bools_Y[1] : new_vars_bools_X[1]);
	    	Draw(false);
	    }*/
	}

}
function toggle_selectall(source, suffix) {
	checkboxes = document.getElementsByClassName('choose_var_box' + suffix);
	for (var i = 0, n = checkboxes.length; i < n; i++)
		checkboxes[i].checked = source.checked;
	window["varselected" + suffix] = new Array(checkboxes.length).fill(source.checked);
	varselected_changed = true;
}



