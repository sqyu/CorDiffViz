# CorDiffViz: an R Package for visualizing multi-omics differential correlation networks
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

# Demo
An interactive demo is available [here](https://diffcornet.github.io/CorDiffViz/demo.html).

# Usage
Install the R package, and in R call the 
```R
CorDiffViz::viz()
```
function on the data matrices; an example is given [here](demo/demo.R). For the arguments of the function, consult the R documentation by calling```?CorDiffViz::viz```. The main data matrices required by the function should have measurements for one sample in each row, and each column should correspond to one variable.

When calling the function, the package automatically estimates the (differential) correlation matrices, and performs permutation and parametric tests as instructed by the user. The user may run the function multiple times (with different arguments) on multiple datasets by assigning a different name to each run; each run can be visualized by selecting it from a dropdown menu in \texttt{viz.html}.

After the R code finishes running, you can open ```viz.html``` in your **current R working directory** from your local browser ([Chrome](https://www.google.com/chrome/)/[Firefox](https://www.mozilla.org/firefox/)) for visualizations of the estimates. Although less interesting, under your **current R working directory**, the ```plots``` folder contains some static graphs and heatmaps for your reference. Data files are located in the ```dats``` folder.

# If you manually changed the auto-generated files
After calling ```CorDiffViz::viz()``` to do the estimation, the visualization file ```viz.html``` and the generated data should automatically be in your current working directory in R. Just open the html from [Chrome](https://www.google.com/chrome/) or [Firefox](https://www.mozilla.org/firefox/) to see the visualizations and nothing else needs to be done. 

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


# Details
In the ```"Correlation Plots" mode```, the matrix in the center represents the raw or thresholded differential or original correlation matrix. The  The square/rectangle represents the matrix, with X variables on the vertical axis, and Y on the horizontal axis. Thus, the entry in the i-th row and j-th column represents cor(X_{k,i},Y_{k,j}) for one population k=1, 2, or their difference.  By hovering the mouse over an entry, its value in the matrix and the corresponding X and Y variables will be displayed; by clicking on the entry, the cell will be locked in and remains static whenever the mouse moves away from the correlation matrix area, and the corresponding scatter plots (for one population or both depending on the selection) will be drawn on the right. The raw correlations are included in the title(s) of the scatterplot(s). Clicking on either axis of the scatterplot(s) swaps the roles of the two variables; hovering over a point shows its corresponding values and the subject ID as specified in the row names of the data matrix provided to ```viz()```.

The second visualization mode, Interactive Networks, is implemented using ```Cytoscape.js```. Each node in the undirected correlation network represents a variable (feature), and an edge is present if the corresponding entry in the (differential) correlation matrix is statistically significant. Multiple network layouts are available. Each node is draggable with size positively related to the number of variables connected to it. If the cross-correlations between two different X and Y variabbles are examined, nodes in the X and Y groups are colored in orange and green, respectively. Clicking on an edge highlights it and hides all other edges, with the (differential) correlation value and the two variables associated with it shown on the right. Clicking on a node highlights the node and edges linked to it while hiding all other edges; the neighbors of the node and the corresponding (differential) correlations are printed on the right, sorted in descending magnitude.
