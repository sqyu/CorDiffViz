library(CorDiffViz)

cat("Estimation using the example code below may take half an hour. The results are already stored", 
    "in the package so you may run the following two lines to copy the files to your current directory",
    "and look at the visualizations by opening viz.html without actually running the code.\n")
cat('file.copy(file.path(path.package("CorDiffViz"), "demo_results", c("dats", "plots")), ".", recursive=TRUE)', "\n")
cat('CorDiffViz::setup_js_html()', "\n")

readline("Press enter to continue executing the code.")

dat0 <- read.csv(file.path(path.package("CorDiffViz"), "extdata/sample_data.csv"))
dat1 <- dat0[dat0$Group=="AA", 2:ncol(dat0)]
dat2 <- dat0[dat0$Group=="BB", 2:ncol(dat0)]
dim(dat1) # 102 x 48
dim(dat2) # 98 x 48

# Self correlations
t1 <- Sys.time()
CorDiffViz::viz(dat_name="self_cors", dat1X=dat1, dat2X=dat2, dat1Y=NULL, dat2Y=NULL, 
                name_dat1="AA", name_dat2="BB", 
                cor_names=c("pearson","spearman", "kendall","sin_spearman","sin_kendall"), 
                permutation=TRUE, alpha=0.05, sides=2, B=1000, adj_method="BY", verbose=TRUE, 
                make_plot=TRUE, parallel=FALSE, perm_seed=1, Cai_seed=1, layout_seed=1)
Sys.time() - t1

# Correlations between variables in group X and variables in group Y, with X and Y equal size
t1 <- Sys.time()
CorDiffViz::viz(dat_name="XY_cors", dat1X=dat1[,1:(ncol(dat1)/2)], dat2X=dat2[,1:(ncol(dat1)/2)], 
                dat1Y=dat1[,(ncol(dat1)/2+1):ncol(dat1)], dat2Y=dat2[,(ncol(dat1)/2+1):ncol(dat1)], 
                name_dat1="AA", name_dat2="BB", 
                cor_names=c("pearson","spearman", "kendall","sin_spearman","sin_kendall"), 
                permutation=TRUE, alpha=0.05, sides=2, B=1000, adj_method="BY", verbose=TRUE, 
                make_plot=TRUE, parallel=FALSE, perm_seed=1, layout_seed=1)
Sys.time() - t1

# Correlations between variables in group X and variables in group Y, with Y twice the size of X
t1 <- Sys.time()
CorDiffViz::viz(dat_name="XY_cors_Ylong", dat1X=dat1[,1:(ncol(dat1)/3)], dat2X=dat2[,1:(ncol(dat1)/3)], 
                dat1Y=dat1[,(ncol(dat1)/3+1):ncol(dat1)], dat2Y=dat2[,(ncol(dat1)/3+1):ncol(dat1)], 
                name_dat1="AA", name_dat2="BB", 
                cor_names=c("pearson","spearman", "kendall","sin_spearman","sin_kendall"), 
                permutation=TRUE, alpha=0.05, sides=2, B=1000, adj_method="BY", verbose=TRUE, 
                make_plot=TRUE, parallel=FALSE, perm_seed=1, layout_seed=1)
Sys.time() - t1

# Correlations between variables in group X and variables in group Y, with X twice the size of Y
t1 <- Sys.time()
CorDiffViz::viz(dat_name="XY_cors_Xlong", dat1X=dat1[,1:(2*ncol(dat1)/3)], dat2X=dat2[,1:(2*ncol(dat1)/3)], 
                dat1Y=dat1[,(2*ncol(dat1)/3+1):ncol(dat1)], dat2Y=dat2[,(2*ncol(dat1)/3+1):ncol(dat1)], 
                name_dat1="AA", name_dat2="BB", 
                cor_names=c("pearson","spearman", "kendall","sin_spearman","sin_kendall"), 
                permutation=TRUE, alpha=0.05, sides=2, B=1000, adj_method="BY", verbose=TRUE, 
                make_plot=TRUE, parallel=FALSE, perm_seed=1, layout_seed=1)
Sys.time() - t1

# You can then open viz.html under the current folder to see the visualization.
# To clean up files generated above, call
# for (d in c("self_cors", "XY_cors", "XY_cors_Ylong", "XY_cors_Xlong")) 
#   unlink(file.path(c("dats", "plots"), d), recursive = TRUE)
# CorDiffViz::setup_js_html()
