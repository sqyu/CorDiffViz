if (folder_name !== "NotFound" && folder_name !== "" && (typeof first_name === 'undefined'))
	alert("The folder name you selected is invalid. Please check that the folder you selected ('dats/" + folder_name + "') exists, and that it contains dat.json, cors.json, diffs.json and graphs.json. If you have manually changed your folder name after using our R code to generate it, please open the html file in an editor, and manually edit the folder list by searching for 'folder_names'.")



for (var radio_text of document.getElementsByClassName('radio_text')) 
	radio_text.style.display = 'inline';
document.getElementById("pleasewait").style.display = 'none';

document.getElementById("d3orcytoscape").style.left = margin.left+"px";
document.getElementById("oneortwo").style.left = margin.left+"px";
document.getElementById("cortype").style.left = margin.left+"px";
var right_side_buttons_left = margin.left + full_width - Math.max(document.getElementById("whichdataid").getBoundingClientRect().width, document.getElementById("testtype").getBoundingClientRect().width, document.getElementById("whichlayout").getBoundingClientRect().width);
document.getElementById("whichlayout").style.left = right_side_buttons_left + "px";
document.getElementById("whichdataid").style.left = right_side_buttons_left + "px";
document.getElementById("testtype").style.left = right_side_buttons_left + "px";


var cortype = "spearman", testtype = "perm", two = 1, d3ORctyo=0, whichdata = "first", whichlayout="grid",
	dataradios = document.getElementById('whichdataid'),
	testcairadio = document.getElementById('testtypecai'),
	testcaitext = document.getElementById('testtypecaitext'),
	testrawradio = document.getElementById('testtyperaw'),
	testrawtext = document.getElementById('testtyperawtext'),
	layoutradios = document.getElementById('whichlayout');

autohideradios();

//switch_div(d3ORctyo);
Draw();

d3.selectAll("input[name='d3orcytoscape']").on("change", function () {
	d3ORctyo = +this.value;
	autohideradios();
	if (d3ORctyo && testtype === "raw")
		toperm("raw");
	toggle_div()//switch_div(d3ORctyo);
	Draw();
})
d3.selectAll("input[name='whichlayout']").on("change", function () {
	whichlayout = this.value;
	Draw();
})
d3.selectAll("input[name='oneortwo']").on("change", function () {
	two = +this.value;
	prefix = +two ? "diff" : "cor";
	autohideradios();
	if (!two) {
		whichdata = getradiovalue(dataradios);
		if (testtype === "cai") 
			toperm("cai");
	}
	Draw();
})
d3.selectAll("input[name='whichdata']").on("change", function () {
	whichdata = this.value;
	Draw();
})
d3.selectAll("input[name='cortype']").on("change", function () {
	cortype = this.value;
	autohideradios();
	if (cortype !== "pearson" && testtype === "cai")
		toperm("cai");
	Draw();
})
d3.selectAll("input[name='testtype']").on("change", function () {
	testtype = this.value;
	Draw();
})
