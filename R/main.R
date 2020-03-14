#library(knitr); library(rmarkdown); library(devtools); library(roxygen2)
#document(); build(); install(); check()

INF <- 1e+6 ## To prevent overflow
CLOSE_TO_ONE <- 1 - 1e-5


check_dat <- function(dat1X, dat2X, dat1Y, dat2Y) {
  if (any(is.na(dat1X)) || any(is.na(dat2X)))
    stop("There must be no missing in dat1X and dat2X.")
  if (ncol(dat1X) != ncol(dat2X))
    stop("dat1X and dat2X must have the same number of columns.")
  if (!all(apply(dat1X, 2, is.numeric)) || !all(apply(dat2X, 2, is.numeric)))
    stop("dat1X and dat2X must both be numeric.")
  if (any(abs(dat1X) >= INF) || any(abs(dat2X) >= INF))
    stop("All entries in dat1X and dat2X must be smaller than ",INF," in absolute value.")
  if (xor(is.null(dat1Y), is.null(dat2Y)))
    stop("dat1Y and dat2Y must be both NULL or both non-NULL.")
  if (!is.null(dat1Y)) {
    if (any(is.na(dat1Y)) || any(is.na(dat2Y)))
      stop("There must be no missing in dat1Y and dat2Y.")
    if (nrow(dat1X) != nrow(dat1Y))
      stop("dat1X and dat1Y must have the same number of rows.")
    if (nrow(dat2X) != nrow(dat2Y))
      stop("dat2X and dat2Y must have the same number of rows.")
    if (ncol(dat1Y) != ncol(dat2Y))
      stop("dat1Y and dat2Y must have the same number of columns.")
    if (!all(apply(dat1Y, 2, is.numeric)) || !all(apply(dat2Y, 2, is.numeric)))
      stop("dat1Y and dat2Y must both be numeric.")
    if (any(abs(dat1Y) >= INF) || any(abs(dat2Y) >= INF))
      stop("All entries in dat1Y and dat2Y must be smaller than ",INF," in absolute value.")
  }
}

get_colnames <- function(colnames1, colnames2, p, suffix) {
  if (xor(is.null(colnames1), is.null(colnames2)))
    return (switch(is.null(colnames1)+1, colnames1, colnames2))
  if (is.null(colnames1))
    return (paste(suffix, 1:p, sep=""))
  if (any(colnames1 != colnames2))
    stop("dat1", suffix, " and dat2", suffix, " have different column names.")
  return (colnames1)
}

pct_ones <- function(preds, Ygiven) {
  if (Ygiven) return (round(100 * mean(preds), 2))
  else return (round(100 * mean(preds[diag(ncol(preds)) == 0]), 2))
}

make_heat_map <- function(mat, cor_name, filename, verbose, plot_folder) {
  file_path <- file.path(plot_folder, cor_name, filename)
  if (verbose) cat("Making heat map ", file_path, ".\n", sep="")
  graphics::par(mar=c(1,1,1,1))
  grDevices::pdf(file_path, 12, 12)
  corrplot::corrplot(mat, tl.cex=0.7, type="upper", addgrid.col = "gainsboro")
  grDevices::dev.off()
}

get_raw_cors <- function(cal_cor, cor_name, dat1X, dat2X, dat1Y, dat2Y, name_dat1, name_dat2, 
                         dat_folder, plot_folder, verbose=TRUE, make_plot=TRUE) {
  ########## Raw ##########
  if (verbose) cat("Calculating raw correlation for", name_dat1, "\n")
  raw_cor1 <- make_cor_mat(cal_cor, dat1X, dat1Y)
  if (verbose) cat("Calculating raw correlation for", name_dat2, "\n")
  raw_cor2 <- make_cor_mat(cal_cor, dat2X, dat2Y)
  if (is.null(dat1Y)) ## Set diag to 0 if self correlation
    diag(raw_cor1) <- diag(raw_cor2) <- 0
  Ygiven <- !is.null(dat1Y)
  cor_json_cor(cor_mats=list(raw_cor1, raw_cor2), 
               cormatnames=c(paste("cor", cor_name, "raw_first", sep="_"), 
                             paste("cor", cor_name, "raw_second", sep="_")),
               filename=file.path(dat_folder, "cors.json"),
               Ygiven=Ygiven)
  if (make_plot) {
    make_heat_map(raw_cor1, cor_name, paste("heatmap", name_dat1, "raw.pdf", sep="_"), verbose, plot_folder)
    make_heat_map(raw_cor2, cor_name, paste("heatmap", name_dat2, "raw.pdf", sep="_"), verbose, plot_folder)
  }
  if (is.null(dat1Y)) ## Set diag back to 1 if self correlation
    diag(raw_cor1) <- diag(raw_cor2) <- 1
  raw_cors <- list(raw_cor1, raw_cor2)
  names(raw_cors) <- c(name_dat1, name_dat2)
  return (raw_cors)
}

get_para_one <- function(raw_cors_safe, cor_name, cor_type, npn, adj_method, 
                         ns, sides, alpha, Ygiven, verbose) {
  if (verbose) {
    cat(rep("*",40),"\n",sep="")
    cat("Parametric tests using", cor_name, "correlation:", "\n")
  }
  cor_to_p <- cor_to_p_function_generator(cor_type=cor_type, npn=npn)
  p_paras <- lapply(1:2, function(samp_i){
    R_to_P(cor_to_p, ns[samp_i], sides, raw_cors_safe[[samp_i]], Ygiven, adj_method)})
  names(p_paras) <- names(raw_cors_safe)
  return (p_paras)
}

get_perm_one <- function(cor_name, cal_cor, sides, name_dat12, datXs, datYs, B, adj_method, parallel, verbose, perm_seed) {
  if (verbose) {
    cat(rep("*",40),"\n",sep="")
    cat("Permutation tests using", cor_name, "correlation:", "\n")
  }
  p_perms <- lapply(1:2, function(samp_i){
    if (verbose) cat(name_dat12[samp_i], ": \n", sep="")
    perm_test(cal_cor, sides, datXs[[samp_i]], datYs[[samp_i]], B=B, adj_method=adj_method, parallel=parallel, verbose=verbose, perm_seed=perm_seed)})
  names(p_perms) <- name_dat12
  return (p_perms)
}

save_print_plot_one <- function(raw_cors, cors, ps, cor_name, mode, Xnames, Ynames, alpha, 
                                verbose, make_plot, dat_folder, plot_folder) {
  # cors: thresholded cor matrices; ps: p-value matrices
  if (!mode %in% c("para", "perm")) stop("save_print_plot_one: mode must be para or perm.")
  Ygiven <- !is.null(Ynames)
  cor_json_cor(cor_mats=cors, 
               cormatnames=c(paste("cor", cor_name, mode, c("first","second"), sep="_")), 
               filename=file.path(dat_folder, "cors.json"), Ygiven=Ygiven)
  graph_json(cors, filename=file.path(dat_folder, "graphs.json"), 
             graph_names=paste("graph", cor_name, mode, c("first", "second"), sep="_"), 
             row_names=Xnames, col_names=Ynames)
  name_dat12 <- names(raw_cors)
  if (verbose) {
    for (samp_i in 1:2) {
      cat("Cor mat for ", name_dat12[samp_i], " has ", pct_ones(ps[[samp_i]] <= alpha, Ygiven), "% significant ", ifelse(Ygiven, "", "off-diagonal "), "entries\n", sep="")
      cat(paste("Significant in ", name_dat12[samp_i], " but not ", name_dat12[3-samp_i], ": ", pct_ones(ps[[samp_i]] <= alpha & ps[[3-samp_i]] > alpha, Ygiven), "%\n", sep=""))
    }
  }
  if (make_plot) {
    cor_diffs <- crossthreshold_mat(raw_cors, ps, alpha)
    for (name_i in name_dat12) {
      make_heat_map(cors[[name_i]], cor_name, paste(paste("heatmap",name_i,mode,sep="_"),".pdf",sep=""), verbose, plot_folder)
      make_heat_map(cor_diffs[[name_i]], cor_name, paste("heatmap",name_i,mode,"crossdiff.pdf",sep="_"), verbose, plot_folder)
    }
  }
}

plot_undirected <- function(mat, cor_name, node_names, filename, main, verbose, plot_folder, layout_seed=NULL) {
  if (length(dim(mat)) != 2 || nrow(mat) != ncol(mat)) 
    stop("plot_undirected: mat must be a square matrix.")
  file_path <- file.path(plot_folder, cor_name, filename)
  if (verbose) cat("Making graph ", file_path, ".\n", sep="")
  colnames(mat) <- rownames(mat) <- node_names
  gr <-  igraph::graph_from_adjacency_matrix(abs(mat) >= 1e-5, mode="undirected", diag=FALSE)
  gr2 <- igraph::delete.vertices(gr, igraph::V(gr)[igraph::degree(gr)==0])
  gr2_ori <- gr2
  # Truncate names; not good for practice as might result in duplicates
  #igraph::V(gr2)$name <- strtrim(gsub("[.]","",igraph::V(gr2_ori)$name), 6)
  grDevices::pdf(file_path, 12, 12)
  graphics::par(mar=c(2,0,2,0))
  graphics::layout(matrix(c(1,2), nrow=1), widths=c(3,1))
  set.seed(layout_seed)
  if (igraph::gsize(gr)){ # If empty graph, don't plot
    graphics::plot(gr2, vertex.size=(igraph::degree(gr2))^(1/3)*3, 
         layout=igraph::layout_nicely(gr2),edge.width=2, 
         vertex.color=grDevices::colorRampPalette(c("yellow","red"))(max(igraph::degree(gr2)))[igraph::degree(gr2)], 
         main=main, type = "n")
    graphics::plot.new()
    gr2_deg <- sort(igraph::degree(gr2), decreasing=TRUE)
    gr2_ori_deg <- sort(igraph::degree(gr2_ori), decreasing=TRUE)
    graphics::legend("topright", legend = paste(names(gr2_ori_deg), gr2_ori_deg, sep=": "), ncol=1, 
           col = grDevices::colorRampPalette(c("yellow","red"))(max(igraph::degree(gr2)))[gr2_deg], 
           x.intersp=0.1, xjust=10, y.intersp=1, pch=16, lty=0, pt.cex=gr2_ori_deg^(1/3)/1.5, lwd=1, 
           cex=1, bty = "n")
  }
  grDevices::dev.off()
}

save_print_plot_diff <- function(mat, cor_name, mode, Xnames, Ynames,
                                 verbose, make_plot, dat_folder, plot_folder, layout_seed=NULL) {
  if (!mode %in% c("para", "perm", "cai", "raw")) stop("save_print_plot_diff: mode must be raw, para, perm or cai.")
  Ygiven <- !is.null(Ynames)
  if (verbose && mode != "raw") cat(pct_ones(mat != 0, Ygiven), "% ", ifelse(Ygiven, "", "off-diagonal "), "entries are significantly different.\n", sep="")
  cor_json_cor(cor_mats=mat, cormatnames=paste("diff", cor_name, mode, sep="_"), 
               filename=file.path(dat_folder, "diffs.json"), Ygiven=Ygiven)
  graph_json(mat, filename=file.path(dat_folder, "graphs.json"), 
             graph_names=paste("graph_diff", cor_name, mode, sep="_"), 
             row_names=Xnames, col_names=Ynames)
  if (make_plot) {
    ### Heat map for differences; divide by 2 to standardize to [-1,1]
    make_heat_map(mat/2, cor_name, paste("heatmap", mode, "diff.pdf", sep="_"), verbose, plot_folder)
    if (mode != "raw" && !Ygiven) ## For self correlations only?? ####
    plot_undirected(mat, cor_name, Xnames, paste("graphmap", mode, "diff.pdf", sep="_"), 
                    paste("Differential network, ", mode, " tests, ", cor_name,", correlations", sep=""), 
                    verbose, plot_folder, layout_seed=layout_seed)
  }
}


#' Main function for estimating and writing self/differential correlation matrices to local files.
#' 
#' Main function for estimating and writing self/differential correlation matrices to local files.
#' 
#' @param dat_name A string, a given name for this dataset. Files for visualization will be saved under \code{file.path("dats", dat_name)}.
#' @param dat1X A matrix data for group X for the first sample; see details. Must not be \code{NULL} and must have the same number of columns as \code{dat2X}.
#' @param dat2X A matrix data for group X for the second sample; see details. Must not be \code{NULL} and must have the same number of columns as \code{dat1X}.
#' @param dat1Y Optional, a matrix data for group Y for the first sample and defaults to \code{NULL}; see details. If not \code{NULL}, must have the same number of rows as \code{dat1X} and same number of columns as \code{dat2Y}, and \code{dat2Y} must not be \code{NULL}.
#' @param dat2Y Optional, a matrix data for group X for the second sample and defaults to \code{NULL}; see details. If not \code{NULL}, must have the same number of rows as \code{dat2X} and same number of columns as \code{dat1Y}, and \code{dat1Y} must not be \code{NULL}.
#' @param name_dat1 A string, name for the first sample. Defaults to "1".
#' @param name_dat2 A string, name for the second sample. Defaults to "2".
#' @param cor_names A string or a vector of strings, name(s) of correlation types to be estimated. Must be chosen from \code{"pearson"}, \code{"kendall"}, \code{"spearman"}, \code{"nonparakendall"}, and \code{"nonparaspearman"}.
#' @param permutation Logical, indicating whether permutation tests should be done in addition to parametric tests; defaults to \code{TRUE}.
#' @param alpha Numerical, the significance level in hypothesis testing; defaults to 0.05.
#' @param sides A number \code{1}, \code{2}, \code{3} or a matrix containing \code{1}, \code{2}, \code{3}. If a matrix, must be of size \code{ncol(dat1X) x ncol(dat1X)} if \code{dat1Y} is \code{NULL}, or \code{ncol(dat1X) x ncol(dat1Y)} otherwise. \code{2} stands for two-sided tests, \code{1} for one-sided test with null hypothesis being the corresponding entries >= 0 (the corresponding correlation for sample 1 stronger than that for sample 2), and \code{3} for one-sided test with null hypothesis being the corresponding entries <= 0.
#' @param B An integer, the number of bootstrapping samples in permutation tests; defaults to 1000.
#' @param adj_method A string, the method passed to \code{stats::p.adjust} for adjusting the p values for multiple testing; defaults to "BY". Must be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "none".
#' @param parallel A logical, whether to use parallel computing; may fail sometimes for some systems and defaults to \code{FALSE}.
#' @param verbose A logical, whether to print progress; defaults to \code{TRUE}.
#' @param make_plot A logical, whether to make heat maps and static graphs; defaults to \code{TRUE}. Plots will be made under \code{file.path("plots", dat_name)}.
#' @param perm_seed A number, seed for permutation test; defaults to \code{NULL}.
#' @param Cai_seed A number, seed for the method by Cai and Zhang; defaults to \code{NULL}.
#' @param layout_seed A number, seed for the layout of the static graphs; defaults to \code{NULL}.
#' @details
#' If \code{dat1Y} and \code{dat2Y} are \code{NULL}, the function estimates the difference \code{cor(dat1X) - cor(dat2X)} and truncates to 0 the entries that are below a certain threshold determined by parameteric/permutation tests.
#' If \code{dat1Y} and \code{dat2Y} are not \code{NULL}, the difference \code{cor(dat1X, dat1Y) - cor(dat2X, dat2Y)} is estimated.
#' @return Does not return anything, but instead creates folders and files under \code{file.path("dats", dat_name)} and \code{file.path("plots", dat_name)}.
#' @examples
#' # For examples, please see the demo in the corresponding script package for visualization.
#' @export
viz <- function(dat_name, dat1X, dat2X, dat1Y=NULL, dat2Y=NULL, name_dat1="1", name_dat2="2",
                cor_names=c("pearson","kendall","spearman","nonparakendall","nonparaspearman"), 
                permutation=TRUE, alpha=0.05, sides=2, B=1000, adj_method="BY",
                parallel=FALSE, verbose=TRUE, make_plot=TRUE, perm_seed=NULL, 
                Cai_seed=NULL, layout_seed=NULL){
  ## In summary: dat1X has dimension n1*px, dat2X n2*px, dat1Y n1*py, dat2Y n2*py
  if (nchar(stringr::str_extract(dat_name, "[a-zA-Z0-9_]+")) != nchar(dat_name))
    stop("dat_name can only contain alphanumerics and underscore.")
  if (name_dat1 == "" || name_dat2 == "" || !is.character(name_dat1) || !is.character(name_dat2) || name_dat1 == name_dat2)
    stop("name_dat1 and name_dat2 must be different nonempty strings.")
  name_dat12 <- c(name_dat1, name_dat2)
  if (is.null(dat1X) || is.null(dat2X))
    stop("dat1X and dat2X cannot be NULL.")
  ## Check dat dimensions and entries
  check_dat(dat1X, dat2X, dat1Y, dat2Y) 
  dat1X <- as.matrix(dat1X); dat2X <- as.matrix(dat2X)
  if (is.null(dat1Y)) {pY <- 0
  } else {
    dat1Y <- as.matrix(dat1Y); dat2Y <- as.matrix(dat2Y)
    pY <- ncol(dat1Y)
  }
  pX <- ncol(dat1X); n1 <- nrow(dat1X); n2 <- nrow(dat2X)
  ## If data for X and Y are the same, remove Y and switch to self-correlation
  if (pX == pY && max(abs(dat1X - dat1Y)) < 1e-14 && max(abs(dat2X - dat2Y)) < 1e-14) {
    warning("dat1X == dat1Y and dat2X == dat2Y. dat1Y and dat2Y changed to NULL automatically.")
    dat1Y <- dat2Y <- NULL; pY <- 0
  }
  ## Sanity check for column names, and assign column names
  Xnames <- colnames(dat1X) <- colnames(dat2X) <- get_colnames(colnames(dat1X), colnames(dat2X), pX, ifelse(pY, "X", ""))
  if (pY) {
    Ynames <- colnames(dat1Y) <- colnames(dat2Y) <- get_colnames(colnames(dat1Y), colnames(dat2Y), pY, "Y")
  } else {Ynames <- NULL}
  
  if (length(sides) != 1) {## If sides is not a scalar
    if (length(dim(sides)) != 2 || any(dim(sides) != c(pX, ifelse(pY, pY, pX))))
      stop("The dimension of sides must be the same as the dimension of the correlation matrix.")
    else if (length(unique(c(sides))) == 1) ## If all entries are the same
      sides <- c(sides)[1] # Use a scalar to speed up
  }
  if (!all(sides %in% c(1, 2, 3)))
      stop("All entries in sides must be 1, 2, or 3.")
  cor_names <- unique(cor_names)
  if (!all(cor_names %in% c("pearson", "kendall", "spearman", "nonparakendall", "nonparaspearman")))
    stop("All of cor_names must be pearson, kendall, spearman, nonparakendall, or nonparaspearman.\n")
  
  dir.create("dats", showWarnings = FALSE)
  dat_folder <- file.path("dats", dat_name)
  if (dir.exists(dat_folder)) {
    cat("Removing folder ", dat_folder, ".\n", sep="")
    unlink(dat_folder, recursive=TRUE)
  }
  dir.create(dat_folder)
  if (make_plot) {
    dir.create("plots", showWarnings = FALSE)
    plot_folder <- file.path("plots", dat_name)
    if (dir.exists(plot_folder)) {
      cat("Removing folder ", plot_folder, ".\n", sep="")
      unlink(plot_folder, recursive=TRUE)
    }
    dir.create(plot_folder)
  } else {plot_folder <- ""}
  dat_json(dat1X, dat2X, dat1Y, dat2Y, name_dat1, name_dat2, file.path(dat_folder, "dat.json"))

  for (cor_ind in 1:length(cor_names)){
    cor_name <- cor_names[cor_ind]
    cor_type <- gsub("nonpara", "", cor_name)
    npn <- grepl("nonpara", cor_name)
    cal_cor <- cal_cor_function_generator(cor_type=cor_type, npn=npn)
    if (verbose) cat(rep("*",80),"\nStarting calculations for ", cor_name,"\n", sep="")
    if (make_plot)
      dir.create(file.path(plot_folder, cor_name), showWarnings = FALSE)
    ############################## One-sample correlations ########################################
    ########## One-sample raw correlations ##########
    raw_cors_safe <- raw_cors <- get_raw_cors(cal_cor, cor_name, dat1X, dat2X, dat1Y, dat2Y, 
                                              name_dat1, name_dat2, dat_folder, plot_folder, verbose, make_plot)
    for (raw_i in 1:2)
      raw_cors_safe[[raw_i]][abs(raw_cors_safe[[raw_i]]) > CLOSE_TO_ONE] <- CLOSE_TO_ONE

    ########## One-sample parametric tests ##########
    p_paras <- get_para_one(raw_cors_safe, cor_name, cor_type, npn, adj_method, 
                             c(n1, n2), sides, alpha, pY, verbose)
    cor_paras <- sapply(name_dat12, function(dat_name){
      threshold_mat(raw_cors[[dat_name]], p_paras[[dat_name]], alpha)}, simplify=FALSE, USE.NAMES=TRUE)
    save_print_plot_one(raw_cors, cor_paras, p_paras, cor_name, "para", Xnames, Ynames, 
                        alpha, verbose, make_plot, dat_folder, plot_folder)
    ########## One-sample permutation tests ##########
    if (permutation){
      one_perm_cache <- list()
      #### Reuse results from kendall/pearson with nonpara=F for nonpara=T, and vice versa
      if (!is.null(one_perm_cache[[cor_type]])) {
        if (verbose)
          cat("Reading cached results for", cor_name, "from", paste(ifelse(grepl("nonpara", cor_name), "", "nonpara"), cor_type, sep=""))
        p_perms <- one_perm_cache[[cor_type]]$p_perms
        cor_perms <- one_perm_cache[[cor_type]]$cor_perms
      } else {
        p_perms <- get_perm_one(cor_name, cal_cor, sides, name_dat12, list(dat1X, dat2X), 
                                list(dat1Y, dat2Y), B, adj_method, parallel, verbose, perm_seed)
        cor_perms <- sapply(name_dat12, function(dat_name){
          threshold_mat(raw_cors[[dat_name]], p_perms[[dat_name]], alpha)}, simplify=FALSE, USE.NAMES=TRUE)
        if (cor_type %in% c("kendall", "spearman") && cor_type %in% cor_names && paste("nonpara", cor_type, sep="") %in% cor_names)
          one_perm_cache[[cor_type]] <- list("p_perms"=p_perms, "cor_perms"=cor_perms)  
      }
      save_print_plot_one(raw_cors, cor_perms, p_perms, cor_name, "perm", Xnames, Ynames, 
                          alpha, verbose, make_plot, dat_folder, plot_folder)
    }

    ############################## Diffs ########################################
    ########## Two-sample difference in raw correlations ##########
    raw_diff <- raw_cors[[1]] - raw_cors[[2]]
    if (!pY) diag(raw_diff) <- 0
    save_print_plot_diff(raw_diff, cor_name, "raw", Xnames, Ynames, verbose, make_plot, dat_folder, plot_folder, layout_seed)
    ########## Two-sample difference in raw correlations using Cai and Zhang ##########
    if (cor_type == "pearson"){
      if (verbose) cat(rep("*",40), "\nTesting difference using Cai and Zhang:\n", sep="")
      Cai_diff <- Cai(dat1X, dat2X, dat1Y, dat2Y, dmax=100, hmax=5, fold=5, verbose=verbose, seed=Cai_seed) ## Pearson only, 11.819681%
      save_print_plot_diff(Cai_diff, cor_name, "cai", Xnames, Ynames, verbose, make_plot, dat_folder, plot_folder, layout_seed)
    }
    ########## Two-sample difference in raw correlations using parametric tests ##########
    if (verbose) cat(rep("*",40), "\nTesting difference using parametric tests:\n", sep="")
    cor2s_to_p <- cor2s_to_p_function_generator(cor_type=cor_type, npn=npn)
    p_para_diff <- rr_diff_para(cor2s_to_p, sides, raw_cors_safe, n1, n2, pY, adj_method=adj_method)
    diff_para <- threshold_mat(raw_diff, p_para_diff, alpha)
    save_print_plot_diff(diff_para, cor_name, "para", Xnames, Ynames, verbose, make_plot, dat_folder, plot_folder, layout_seed)
    ########## Two-sample difference in raw correlations using permutation tests ##########
    if (permutation){
      diff_perm_cache <- list()
      if (verbose) cat(rep("*",40), "\nTesting difference using permutation tests:\n", sep="")
      #### Reuse results from kendall/pearson with nonpara=F for nonpara=T, and vice versa
      if (!is.null(diff_perm_cache[[cor_type]])) {
        if (verbose) cat("Reading cached results for", cor_name, "from", paste(ifelse(grepl("nonpara", cor_name), "", "nonpara"), cor_type, sep=""))
        p_perm_diff <- diff_perm_cache[[cor_type]]$p_perm_diff
        diff_perm <- diff_perm_cache[[cor_type]]$diff_perm
      } else {
        p_perm_diff <- perm_test_diff(cal_cor, sides, dat1X, dat2X, dat1Y, dat2Y, B=B, adj_method=adj_method, parallel=parallel, verbose=verbose, perm_seed=perm_seed)
        diff_perm <- threshold_mat(raw_diff, p_perm_diff, alpha)
        if (cor_type %in% c("kendall", "spearman") && cor_type %in% cor_names && paste("nonpara", cor_type, sep="") %in% cor_names)
          diff_perm_cache[[cor_type]] <- list("p_perm_diff"=p_perm_diff, "diff_perm"=diff_perm)  
      }
      save_print_plot_diff(diff_perm, cor_name, "perm", Xnames, Ynames, verbose, make_plot, dat_folder, plot_folder, layout_seed)
    }
    
    if (verbose) cat("Calculations for ", cor_name, " correlations done.","\n", rep("*",80),"\n",sep="")
  }
  
  setup_js_html(dat_name)
  if (verbose) cat("Done.\n")
}