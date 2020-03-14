function toggle_div() {
	var d3_div = document.getElementById("D3"); 
	var cy_div = document.getElementById("CY");
	if (d3_div.style.display !== "none") {
		d3_div.style.display = "none"; 
		cy_div.style.display = "inline"; 
	} else {
		d3_div.style.display = "inline"; 
		cy_div.style.display = "none"; 
	}
}


function Draw() {
	console.log(whichlayout);
	if (d3ORctyo) {
		drawCyto(cortype, testtype, two, datafilename(cortype,testtype,two,whichdata), whichdata, whichlayout);
	}
	else {
		drawD3(cortype, testtype, two, datafilename(cortype,testtype,two,whichdata), whichdata);
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