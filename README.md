# CorDiffViz
Code for estimating and visualizing differential correlation matrices for two samples.

# Installation
Run the following lines in R.
```R
install.packages("devtools")
devtools::install_github("sqyu/CorDiffViz")
```

# What this package does
Sparsely estimates and visualizes
```R
cor(dat1) - cor(dat2)
```
(self correlations), or 
```R
cor(dat1X, dat1Y) - cor(dat2X, dat2Y)
```
(cross correlations) with thresholds determined by parametric tests, permutation tests, and the test from Cai and Zhang (2016).

# Usage
Install the R package, and in R call the 
```R
CorDiffViz::viz()
```
function on the data matrices; an example is given [here](demo/demo.R). For the arguments of the function, consult the R documentation by calling```?CorDiffViz::viz```.

After the R code finishes running, you can open ```viz.html``` in your current R working directory from your local browser for visualizations of the estimates. Although less interesting, the ```plots``` folder contains some static graphs and heatmaps for your reference.

# If you manually changed the auto-generated files
After calling ```CorDiffViz::viz()``` to do the estimation, the visualization file ```viz.html``` and the generated data should automatically be in your current working directory in R. Just open it to see the visualizations and nothing else needs to be done. 

If you for some reason changed the names or deleted some of the generated files, you may need to read the following.
```
 📂my_experiment_folder
 ┣ 📜viz.html
 ┣ 📂scripts
 ┃ ┗ 📜.js files and style.css
 ┣ 📂dats
 ┃ ┣ 📂my_data_self_correlations
 ┃ ┃ ┗ 📜.json files
 ┃ ┣ 📂my_data_X1:15_Y16:35_correlations
 ┃ ┃ ┗ 📜.json files
 ┃ ┗ 📂estimates_for_another_data
 ┃   ┗ 📜.json files
 ┗ 📂plots
   ┣ 📂my_data_self_correlations
   ┣ 📂my_data_X1:15_Y16:35_correlations
   ┗ 📂estimates_for_another_data
```

The ```viz.html``` file you are trying to open and its auxiliary ```scripts``` folder must be in the same folder (```my_experiment_folder``` above) as the auto-generated ```dats``` folder. The ```dats``` folder contains sub-folders with names provided by you as the ```dat_name``` argument to ```CorDiffViz::viz()```, and each sub-folder represents one dataset/set of estimates. 

If you delete some of the sub-folders in ```dats``` or change their names you must run ```CorDiffViz::setup_js_html()``` from the directory that contains ```viz.html```, ```scripts``` and ```dats```, i.e. from ```my_experiment_folder``` in the example above.

Similarly, if for some reason your ```viz.html``` and ```scripts``` are missing, simply run ```CorDiffViz::setup_js_html()``` from the folder where ```dats``` lies, i.e. from ```my_experiment_folder``` in the example above.
