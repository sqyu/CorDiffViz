#for (pack in c("corrplot", "igraph", "rjson", "ccaPP", "parallel")){
#  if (!require(pack, character.only = TRUE)){
#    install.packages(pack, dep=TRUE)
#    require(pack, character.only = TRUE)
#  }
#}

writetojson <- function(json, filename){
  fileConn <- file(filename)
  writeLines(json, fileConn)
  close(fileConn)
}

readfromjson <- function(filename){
  if (!file.exists(filename)){
    return (list())
  }
  text <- readLines(filename)
  result <- list()
  for (tt in text){
    sp <- strsplit(tt, " ")[[1]]
    result[[sp[2]]] <- rjson::fromJSON(sp[4])
  }
  return (result)
}

dat_json <- function(dat1X, dat2X, dat1Y, dat2Y, name1, name2, filename){
  ## name1, name2: Name of dat1(X,Y) and dat2(X,Y), respectively
  if (ncol(dat1X) != ncol(dat2X) || length(colnames(dat1X)) != length(colnames(dat2X)) || any(colnames(dat1X) != colnames(dat2X))) 
    stop ("dat1X and dat2X have different number of columns or the column names do not match.")
  ## Test if length(colnames(dat1X)) != length(colnames(dat2X)) since one might be NULL while the other is not
  if (xor(is.null(dat1Y), is.null(dat2Y))) stop("dat1Y and dat2Y must be NULL at the same time.")
  if (!is.null(dat1Y)){
    if (nrow(dat1X) != nrow(dat1Y) || nrow(dat2X) != nrow(dat2Y)) stop("X and Y must have the same number of rows for each condition.")
    if (ncol(dat1Y) != ncol(dat2Y) || length(colnames(dat1Y)) != length(colnames(dat2Y)) || any(colnames(dat1Y) != colnames(dat2Y))) stop("dat1Y and dat2Y have different number of columns or the column names do not match.")
  }
  ret1X <- ret2X <- list()
  ret1X$ind <- paste(1:nrow(dat1X))
  ret2X$ind <- paste(1:nrow(dat2X))
  varsX <- colnames(dat1X)
  ret1X$dat <- ret2X$dat <- list()
  colnames(dat1X) <- colnames(dat2X) <- NULL; 
  rownames(dat1X) <- NULL; rownames(dat2X) <- NULL
  for (i in 1:ncol(dat1X)){
    ret1X$dat[[i]] <- dat1X[,i]
    ret2X$dat[[i]] <- dat2X[,i]
  }
  if (!is.null(dat1Y)){
    ret1Y <- ret2Y <- list()
    ret1Y$ind <- paste(1:nrow(dat1Y))
    ret2Y$ind <- paste(1:nrow(dat2Y))
    varsY <- colnames(dat1Y)
    ret1Y$dat <- ret2Y$dat <- list()
    colnames(dat1Y) <- colnames(dat2Y) <- NULL; 
    rownames(dat1Y) <- NULL; rownames(dat2Y) <- NULL
    for (i in 1:ncol(dat1Y)){
      ret1Y$dat[[i]] <- dat1Y[,i]
      ret2Y$dat[[i]] <- dat2Y[,i]
    }
    writetojson(paste(paste("var first_name = \"",name1,"\"",sep=""),
                      paste("var second_name = \"",name2,"\"",sep=""),
                      paste("var vars_X =",rjson::toJSON(varsX)),
                      paste("var vars_Y =",rjson::toJSON(varsY)),
                      paste(paste("var dat_first_X",sep=""),"=",rjson::toJSON(ret1X)),
                      paste(paste("var dat_second_X",sep=""),"=",rjson::toJSON(ret2X)), 
                      paste(paste("var dat_first_Y",sep=""),"=",rjson::toJSON(ret1Y)),
                      paste(paste("var dat_second_Y",sep=""),"=",rjson::toJSON(ret2Y)), 
                      sep="\n"),
                filename)
  } else { # If Y not given
    writetojson(paste(paste("var first_name = \"",name1,"\"",sep=""),
                    paste("var second_name = \"",name2,"\"",sep=""),
                    paste("var vars =",rjson::toJSON(varsX)),
                    paste(paste("var dat_first",sep=""),"=",rjson::toJSON(ret1X)),
                    paste(paste("var dat_second",sep=""),"=",rjson::toJSON(ret2X)), 
                    sep="\n"),
              filename)
  }
}

graph_json <- function(mats, filename, graph_names, row_names, col_names=NULL, tol=1e-10) {
  dups <- unique(c(row_names, col_names)[duplicated(c(row_names, col_names))])
  if (length(dups))
    stop("The following node names are duplicated: ", dups, ".")
  if (is.matrix(mats)) mats <- list(mats)
  if (length(graph_names) != length(mats))
    stop("mats and graph_names must have the same length.")
  if (length(unique(graph_names)) < length(graph_names))
    stop("Duplicates exist in graph_names: ", graph_names, ".")
  colors <- grDevices::colorRampPalette(c("#DC143C", "white", "#6A5ACD"))(201) # red and slateblue in javascript code
  get_color <- function(cor) {colors[round((cor+1)*100)+1]}
  existing <- readfromjson(filename)
  all_var_names <- c(row_names, col_names)
  var_groups <- c(rep("X", length(row_names)), rep("Y", length(col_names)))
  existing[["graph_nodes"]] <- lapply(1:length(all_var_names), function(vi){
    list("data"=list("id"=all_var_names[vi]), "group"="nodes", "removed"=FALSE, 
         "selected"=FALSE, "selectable"=TRUE, "locked"=FALSE, "grabbed"=FALSE, 
         "grabbable"=TRUE, "classes"=var_groups[vi])
  })
  for (i in 1:length(mats)){
    graph_name <- paste(graph_names[i], "_edges", sep="")
    mat <- mats[[i]]
    if (!is.matrix(mat)) stop("mats must be a matrix or a list of matrices.")
    if (nrow(mat) != length(row_names))
      stop("The ", i, "-th matrix must have number of rows equal to length(row_names). Got nrow(mat) = ", nrow(mat), " but length(row_names) = ", length(row_names), ".")
    adj <- mat != 0
    if (is.null(col_names)) {
      if (ncol(mat) != length(row_names))
        stop("The ", i, "-th matrix must have number of columns equal to length(row_names). Got ncol(mat) = ", ncol(mat), " but length(row_names) = ", length(row_names), ".")
      if (any(abs(mat - t(mat)) > tol))
        stop("The ", i, "-th matrix: mat must be symmetric.")
      edges <- which(adj & upper.tri(adj), arr.ind=TRUE)
    } else {
      if (ncol(mat) != length(col_names))
        stop("The ", i, "-th matrix must have number of columns equal to length(col_names). Got ncol(mat) = ", ncol(mat), " but length(col_names) = ", length(col_names), ".")
      edges <- which(adj, arr.ind=TRUE)
    }
    existing[[paste(graph_names[i], "_maxDegree", sep="")]] <- max(c(colSums(adj), rowSums(adj)))
    existing[[graph_name]] <- c(existing[[graph_name]], 
       apply(which(adj & ((!is.null(col_names)) | upper.tri(adj)), arr.ind=TRUE), 1, 
             function(pair) {
               list("data"=list("id"=paste("edge",pair[1],pair[2],sep="_"),
                                "source"=row_names[pair[1]],
                                "target"=ifelse(is.null(col_names), row_names[pair[2]], col_names[pair[2]]),
                          "value"=mat[pair[1],pair[2]], "color"=get_color(mat[pair[1],pair[2]])),
              "group"="edges", "removed"=FALSE, "selected"=FALSE, "selectable"=TRUE, 
              "locked"=FALSE, "grabbed"=FALSE, "grabbable"=TRUE, "classes"="")
         }))
  }
  writetojson(paste(sapply(sort(names(existing)), function(x){
    paste("var",x,"=",rjson::toJSON(existing[[x]]))}),
    collapse="\n"), filename)
}

#' Sets up the html and javascript scripts in the current folder for visualization.
#' 
#' Sets up the html and javascript scripts in the current folder for visualization.
#' 
#' @details
#' Copies inst/scripts and inst/viz.html from package to the current folder.
#' Then inserts subfolder names under "dats/" that contain "cors.json", "dat.json", "diffs.json" and "graphs.json" in viz.html between the lines "folder_names = [" and "]".
#' @return Does not return anything.
#' @examples
#' setup_js_html()
#' @export
setup_js_html <- function(){
  pack_dir <- path.package("CorDiffViz")
  if (!dir.exists("scripts")) {
    file.copy(file.path(pack_dir, "scripts"), ".", recursive=TRUE)
  }
  if (!file.exists("viz.html"))
    file.copy(file.path(pack_dir, "viz.html"), "viz.html")
  f <- file("viz.html", "r"); g <- file("viz_tmp.html", "w")
  while (TRUE) {
    line <- readLines(f, n=1)
    if (length(line) == 0) break
    if (grepl("folder_names\\s*=", line)) {
      writeLines("folder_names = [", g)
      dat_names <- Filter(function(f){dir.exists(file.path("dats", f)) &&
          all(c("cors.json", "dat.json", "diffs.json", "graphs.json") %in% list.files(file.path("dats", f)))},
          list.files("dats"))
      writeLines(c(paste("\t\"", unique(dat_names), "\"", collapse=",\n", sep=""), "]"), g)
      while (!grepl("^\\s*\\]", line)) # Skip all lines until hitting the first "]"
        line <- readLines(f, n=1)
    } else
      writeLines(line, g)
  }
  close(f); close(g)
  file.remove("viz.html")
  file.rename("viz_tmp.html", "viz.html")
}

############################################

cor_json_cor <- function(cor_mats, cormatnames, filename, Ygiven){
  ## cor_mats: list of correlation matrices, or a single cor mat
  ## cormatnames: vector of names for matrices in cor_mats
  existing <- readfromjson(filename)
  if (typeof(cor_mats) != "list")
    cor_mats <- list(cor_mats)
  if (length(cor_mats) != length(cormatnames))
    stop("cor_mats and cormatnames should have the same length.")
  for (i in 1:length(cor_mats)){
    cormatname <- cormatnames[i]
    cor_mat <- cor_mats[[i]]
    if ((!Ygiven) && nrow(cor_mat) != ncol(cor_mat)) stop("cor_mat must be a square matrix.")
    corr <- list()
    colnames(cor_mat) <- rownames(cor_mat) <- NULL
    for (i in 1:ncol(cor_mat)){
      corr[[i]] <- cor_mat[,i]
    }
    ### stores cor mat with name cormatname
    existing[[cormatname]] <- corr
    ### stores proportion of non-zero off-diagonal entries with name prop_cormatname
    if (Ygiven)
      existing[[paste("prop_",cormatname,sep="")]] <- as.double(sprintf("%.6f", 100*mean(cor_mat!=0)))
    else
      existing[[paste("prop_",cormatname,sep="")]] <- as.double(sprintf("%.6f", 100*mean(cor_mat[diag(ncol(cor_mat))==0]!=0)))
  }
  writetojson(paste(sapply(sort(names(existing)), function(x){paste("var",x,"=",rjson::toJSON(existing[[x]]))}), 
                    collapse="\n"),
              filename)
}

p_perm_gen <- list(function(perm_cors, true_cor){mean(perm_cors <= true_cor)},
                   function(perm_cors, true_cor){mean(abs(perm_cors) >= abs(true_cor))},
                   function(perm_cors, true_cor){mean(perm_cors >= true_cor)})

p_para_gen <- list(identity,
                   function(cdf){1-2*abs(cdf-0.5)},
                   function(cdf){1-cdf})

cal_cor_function_generator <- function(cor_type, npn=FALSE){
  if (cor_type == "pearson"){
    return (function(x,y){ccaPP::corPearson(x,y)})
  } else if (cor_type == "kendall"){
    return (function(x,y){ccaPP::corKendall(x, y, consistent = npn)})
  } else if (cor_type == "spearman"){
    return (function(x,y){ccaPP::corSpearman(x, y, consistent = npn)})
  } else {
    stop("Wrong correlation type.")
  }
}

make_cor_mat <- function(cal_cor, datX, datY=NULL){
  # for loop surprisingly faster than sapply, vapply, lapply, parallel::mclapply in experiments
  if (is.null(datY)){
    # Self correlation
    p <- ncol(datX)
    res <- diag(p)/2
    for (i in 1:(p-1)){
      for (j in (i+1):p){
        res[i,j] <- cal_cor(datX[,i], datX[,j])
      }
    }
    return (res + t(res))
  } else {
    # Cross correlation
    if (nrow(datX) != nrow(datY))
      stop("datX and datY must have the same number of rows.")
    pX <- ncol(datX)
    pY <- ncol(datY)
    res <- matrix(nrow=pX, ncol=pY)
    for (i in 1:pX){
      for (j in 1:pY){
        res[i,j] <- cal_cor(datX[,i], datY[,j])
      }
    }
    return (res)
  }
}

adjust_ps <- function(p_mat, adj_method, Ygiven){
  if (!Ygiven){ # Self correlation
    p_adj <- diag(ncol(p_mat))
    p_adj[upper.tri(p_adj)] <- stats::p.adjust(p=p_mat[upper.tri(p_adj)],method=adj_method)
    p_adj <- (p_adj + t(p_adj))
    diag(p_adj) <- 1
    return (p_adj)
  } else {
    return (matrix(stats::p.adjust(p=p_mat,method=adj_method), nrow=nrow(p_mat), ncol=ncol(p_mat)))
  }
}


cdf_to_p_para_vec <- Vectorize(
  function(cdf, side){
    p_para_gen[[side]](cdf)
  })
cdf_to_p_para_mat <- function(cdfs, sides){
  # sides: matrix with same dim as cdfs of 1,2,3, or an integer 1,2,3
  if (length(sides)==1){p_para_gen[[sides]](cdfs)} # faster if scalar
  else {matrix(cdf_to_p_para_vec(cdfs, sides), nrow(cdfs), ncol(cdfs))}
}
  
cor_to_p_function_generator <- function(cor_type, npn){
  # r: matrix
  if (cor_type == "pearson") {
    function(r, n){stats::pnorm(atanh(abs(r))*sqrt(n-3))}
    #Alternate: list("f"=function(r,n,c){2*(1-pf((1+abs(r))/(1-abs(r)),n-2,n-2))},
  } else if (cor_type == "kendall") {
    if (npn)
      function(r, n){stats::pnorm(abs(r)*2/pi*sqrt(9*n*(n-1)/2/(2*n+5)))}
    else
      function(r, n){stats::pnorm(abs(r)*sqrt(9*n*(n-1)/2/(2*n+5)))}
  } else if (cor_type == "spearman") {
    if (npn)
      function(r, n){stats::pnorm(abs(r)/pi*3*sqrt(n-2))}
       #2*(1-stats::pnorm(abs(2*sin(r*pi/6)),0,pi/3/sqrt(n-3)*sqrt(1.06)))
    else
      function(r, n){stats::pt(abs(r)/sqrt(1-r^2)*sqrt(n-2), n-2)}
       #2*(1-stats::pnorm(atanh(r)*sqrt((n-3)/1.06),0,1))
  } else
    stop("Wrong correlation type specified.")
}

R_to_P <- function(cor_to_p, n, sides, cor_mat, Ygiven, adj_method="BY"){
  ### entry-wise p values using parameteric distributions for rho ###
  #if (npn && cor_type == "pearson") stop("Nonparanormal only supported for Kendall and Spearman.")
  if ((!Ygiven) && ncol(cor_mat) != nrow(cor_mat)) 
    stop("cor_mat must be a square matrix.")
  return (adjust_ps(cdf_to_p_para_mat(cor_to_p(cor_mat, n), sides), adj_method, Ygiven))
}


threshold_mat <- function(cor_mat, p_mat, alpha){
  if (any(dim(cor_mat) != dim(p_mat)))
    stop("cor_mat and p_mat must have the same shape.")
  cor_mat[p_mat > alpha] <- 0
  return (cor_mat)
}

crossthreshold_mat <- function(cor_mats, p_mats, alpha){
  if (nrow(unique(t(sapply(c(cor_mats, p_mats), dim)))) != 1)
    stop("All matrices in cor_mats and p_mats must have the same shape.")
  cor_para_diffs <- cor_mats
  cor_para_diffs[[1]][p_mats[[1]] > alpha | p_mats[[2]] <= alpha] <- 0
  cor_para_diffs[[2]][p_mats[[2]] > alpha | p_mats[[1]] <= alpha] <- 0
  return (cor_para_diffs)
}

rr_to_p <- function (cal_cor, X, Y, b, p_perm){
  ## Permutation test; npn does not matter here because of monotonicity
  #set.seed(sum(X)+sum(Y)+b)
  return (p_perm(replicate(b, cal_cor(sample(X),Y)), cal_cor(X,Y)))
}

perm_test <- function(cal_cor, sides, datX, datY, B=1000, adj_method="BY", parallel=TRUE, 
                      verbose=TRUE, perm_seed=NULL){
  # sides: scalar or matrix of 1, 2, 3
  set.seed(perm_seed)
  side_scalar <- (length(sides) == 1) # Whether sides if a scalar
  count <- 1
  if (parallel) apply_func <- function(X, FUN) {unlist(parallel::mclapply(X, FUN, mc.preschedule=FALSE))} else apply_func <- sapply
  if (is.null(datY)){
    p <- ncol(datX)
    if (!side_scalar && any(dim(sides)!=p)) {stop("Sides must be a scalar of a ",p,"*",p," matrix.")}
    p_mat <- matrix(0,nrow=p,ncol=p)
    if (verbose) {pb <- utils::txtProgressBar(min = 0, max = p*(p-1)/2, style = 3)}
    for (i in 1:(p-1)){
      p_mat[i, (i+1):p] <- apply_func((i+1):p, function(j){
        side <- ifelse(side_scalar, sides, sides[i,j])
        p_mat[i,j] <- rr_to_p(cal_cor, datX[,i], datX[,j], B, p_perm_gen[[side]])
      })
      if (verbose) {utils::setTxtProgressBar(pb, count); count <- count+p-i}
    }
    p_mat <- (p_mat + t(p_mat))
  } else {
    pX <- ncol(datX)
    pY <- ncol(datY)
    if (!side_scalar && any(dim(sides)!=c(pX, pY))) {stop("Sides must be a scalar of a ",pX,"*",pY," matrix.")}
    p_mat <- matrix(0,nrow=pX,ncol=pY)
    if (verbose) {pb <- utils::txtProgressBar(min = 0, max = pX, style = 3)}
    for (i in 1:pX){
      p_mat[i,] <- apply_func(1:pY, function(j){
        side <- ifelse(side_scalar, sides, sides[i,j])
        rr_to_p(cal_cor, datX[,i], datY[,j], B, p_perm_gen[[side]])
      })
      if (verbose) {utils::setTxtProgressBar(pb, count); count <- count+1}
    }
  }
  if (verbose) cat("\n")  ## After the progressbar ends
  p_adj <- adjust_ps(p_mat, adj_method, !is.null(datY))
  return (p_adj)
}

##### differential
atanh_difference <- function(r1,r2){
  ## r1 and r2 might be 1+epsilon due to rounding errors while while atanh(1+2e-16)=NaN
  r1 <- min(1,max(r1,-1)); r2 <- min(1,max(r2,-1))
  ## Need to rule out atanh(1)-atanh(1) and atanh(-1)-atanh(-1)
  if (r1==r2){0} else{atanh(r1)-atanh(r2)}
}
atanh_difference_vec <- Vectorize(atanh_difference)

atanh_difference_mat <- function(r1s, r2s){ ## matrix version
  matrix(atanh_difference_vec(r1s, r2s), nrow(r1s), ncol(r1s))
}

cor2s_to_p_function_generator <- function(cor_type, npn){
  if (cor_type == "pearson") {
    function(r1,r2,n1,n2){c <- sqrt(1/(n1-3)+1/(n2-3))
    stats::pnorm(abs(atanh_difference_mat(r1,r2)) / c)}
  } else if (cor_type == "kendall") {
    if (npn)
      function(r1,r2,n1,n2){c <- pi/2*(sqrt(2*(2*n1+5)/(9*n1*(n1-1))+2*(2*n2+5)/(9*n2*(n2-1))))
      stats::pnorm(abs(r1-r2) / c)}
    else
      function(r1,r2,n1,n2){c <- sqrt(2*(2*n1+5)/(9*n1*(n1-1))+2*(2*n2+5)/(9*n2*(n2-1)))
      stats::pnorm(abs(r1-r2) / c)}
  } else if (cor_type == "spearman") {
    if (npn)
      function(r1,r2,n1,n2){c <- pi/3*(1/sqrt(n1-2)+1/sqrt(n2-2)); stats::pnorm(abs(r1-r2)/c)}
    else
      function(r1,r2,n1,n2){stats::pnorm(abs(r1*sqrt((n1-2)/(1-r1^2))-r2*sqrt((n2-2)/(1-r2^2))),0,sqrt(2))} #2*(1-stats::pnorm(abs(atanh(cor1)-atanh(cor2)),0,sqrt(1.06/(n1-3)+1.06/(n2-3))))##??stats::pnorm(atanh(rho)*sqrt((n-3)/1.06),0,1))})
  } else {
    stop("Wrong correlation type specified.")
  }
}

rr_diff_para <- function(cor2s_to_p, sides, cors, n1, n2, Ygiven, adj_method="BY"){
  return (adjust_ps(cdf_to_p_para_mat(cor2s_to_p(cors[[1]], cors[[2]], n1, n2), sides), 
    adj_method, Ygiven))
}

rr_diff_perm <- function (cal_cor, X1, Y1, X2, Y2, b, p_perm, use_atanh=TRUE){
  # use_atanh should be TRUE only if cal_cor is pearson or nonparanormal kendall/spearman
  #set.seed(sum(X1)+sum(Y1)+sum(X2)+sum(Y2)+b)
  X <- c(X1,X2); Y <- c(Y1,Y2)
  if (use_atanh) {
    trans <- atanh_difference
  } else {trans<-function(r1,r2){r1-r2}}
  cors <- replicate(b, {ind<-sample(length(X),length(X1));trans(cal_cor(X[ind],Y[ind]),cal_cor(X[-ind],Y[-ind]))})
  true_cor <- trans(cal_cor(X1,Y1),cal_cor(X2,Y2))
  return (p_perm(cors, true_cor))
}

perm_test_diff <- function(cal_cor, sides, X1, X2, Y1=NULL, Y2=NULL, B=1000, adj_method="BY", parallel=TRUE,
                           verbose=TRUE, perm_seed=NULL){
  # cal_cor should have npn=T for kendall and pearson, otherwise transformation not valid
  set.seed(perm_seed)
  if (ncol(X1) != ncol(X2))
    stop("dat1 and dat2 must have the same number of columns.")
  if (is.null(Y1)){if (!is.null(Y2)){stop("Y1 and Y2 must be both NULL or both non-NULL.")}}
  else {if (is.null(Y2)){stop("Y1 and Y2 must be both NULL or both non-NULL.")}
    else {if (ncol(Y1)!=ncol(Y2)){stop("Y1 and Y2 must have the same number of columns.")}
    if (nrow(X1)!=nrow(Y1)){stop("X1 and Y1 must have the same number of rows.")}
    if (nrow(X2)!=nrow(Y2)){stop("X2 and Y2 must have the same number of rows.")}}}
  count <- 1
  side_scalar <- length(sides) == 1
  if (parallel) apply_func <- function(X, FUN) {unlist(parallel::mclapply(X, FUN, mc.preschedule=FALSE))} else apply_func <- sapply
  if (is.null(Y1)){
    p <- ncol(X1)
    if (!side_scalar && any(dim(sides)!=p)) {stop("Sides must be a scalar of a ",p,"*",p," matrix.")}
    p_mat <- matrix(0,nrow=p,ncol=p)
    if (verbose) {pb <- utils::txtProgressBar(min = 0, max = p*(p-1)/2, style = 3)}
    for (i in 1:(p-1)){
      p_mat[i, (i+1):p] <- apply_func((i+1):p, function(j) {
        side <- ifelse(side_scalar, sides, sides[i,j])
        p_mat[i,j] <- rr_diff_perm(cal_cor,X1[,i],X1[,j],X2[,i],X2[,j],B,p_perm_gen[[side]],use_atanh=TRUE)
      })
      if (verbose) {utils::setTxtProgressBar(pb, count); count <- count+p-i}
    }
    p_mat <- (p_mat + t(p_mat))
  } else {
    pX <- ncol(X1); pY <- ncol(Y1)
    if (!side_scalar && any(dim(sides)!=c(pX,pY))) {stop("Sides must be a scalar of a ",pX,"*",pY," matrix.")}
    p_mat <- matrix(0,nrow=pX,ncol=pY)
    if (verbose) {pb <- utils::txtProgressBar(min = 0, max = pX, style = 3)}
    for (i in 1:pX){
      p_mat[i,] <- apply_func(1:pY, function(j) {
        side <- ifelse(side_scalar, sides, sides[i,j])
        p_mat[i,j] <- rr_diff_perm(cal_cor,X1[,i],Y1[,j],X2[,i],Y2[,j],B,p_perm_gen[[side]],use_atanh=TRUE)
      })
      if (verbose) {utils::setTxtProgressBar(pb, count); count <- count+1}
    }
  }
  if (verbose) cat("\n")  ## After the progressbar ends
  p_adj <- adjust_ps(p_mat, adj_method, !is.null(Y1))
  return (p_adj)
}
