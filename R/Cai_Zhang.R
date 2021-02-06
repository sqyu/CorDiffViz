# Slightly adapted from code provided by Anru Zhang anru.stat@gmail.com
# Cai, T. T., & Zhang, A. (2016). Inference for high-dimensional differential correlation matrices. Journal of multivariate analysis, 143, 107-126.


sample.cov <- function(X, Y=NULL){ # Calculate sample covariance matrix
  n <- nrow(X)
  Xr <- X - t(matrix(colMeans(X), ncol(X), n))
  if (is.null(Y)){
    return(t(Xr)%*%Xr/n)
  } else {
    if (nrow(Y) != n) stop("X and Y must have the same number of rows.")
    Yr <- Y - t(matrix(colMeans(Y), ncol(Y), n))
    return(t(Xr)%*%Yr/n)
  }
}


sample.cor <- function(X, Y=NULL){ # Calculate sample correlation matrix
  if (is.null(Y)){
    covhat <- sample.cov(X,Y)
    corhat <- covhat/sqrt(diag(covhat)); corhat <- t(corhat)/sqrt(diag(covhat))
  } else {
    n <- nrow(X)
    if (nrow(Y) != n) stop("X and Y must have the same number of rows.")
    Xr <- X - t(matrix(colMeans(X), ncol(X), n))
    Yr <- Y - t(matrix(colMeans(Y), ncol(Y), n))
    covhat <- t(Xr)%*%Yr/n
    corhat <- t(covhat)/sqrt(colMeans(Yr^2))
    corhat <- t(corhat)/sqrt(colMeans(Xr^2))
  }
  return(corhat)
}

sample.cor_theta_xi_t <- function(X, Y=NULL){ # Calculate sample theta matrix (defined in paper)
  n <- nrow(X); pX <- ncol(X)
  Xr <- X - t(matrix(colMeans(X), pX, n))
  if (is.null(Y)){
    covhat <- sample.cov(X)
    corhat <- covhat/sqrt(diag(covhat)); corhat <- t(corhat)/sqrt(diag(covhat))
    thetahat <- matrix(0, pX, pX)
    for (k in 1:n){
      thetahat <- thetahat + (tcrossprod(Xr[k,]) - covhat)^2/n
    }
    xihat <- thetahat/diag(covhat); xihat <- t(xihat)/diag(covhat)
    sqxihat <- sqrt(xihat)
    that <- (sqxihat+abs(corhat)/2
                *(matrix(diag(sqxihat),pX,pX)+
                  t(matrix(diag(sqxihat),pX,pX)))) / sqrt(n) ####
    ## Implementation by Zhang: 
    ##that <- sqrt(xihat/n) + corhat/2*(matrix(diag(xihat)/n, pX, pX) + t(matrix(diag(xihat)/n, pX, pX))) ####
  } else {
    pY <- ncol(Y)
    Yr <- Y - t(matrix(colMeans(Y), pY, n))
    covhat <- t(Xr)%*%Yr/n
    varXs <- colMeans(Xr^2); varYs <- colMeans(Yr^2) # diag of cov(X) and cov(Y)
    corhat <- t(covhat)/sqrt(varYs); corhat <- t(corhat)/sqrt(varXs)
    thetahat <- matrix(0, pX, pY)
    for (k in 1:n){
      thetahat <- thetahat + (tcrossprod(Xr[k,], Yr[k,]) - covhat)^2/n
    }
    thetaXs <- colMeans((Xr^2-t(matrix(varXs, pX, n)))^2)
    thetaYs <- colMeans((Yr^2-t(matrix(varYs, pY, n)))^2)
    xihat <- t(thetahat)/varYs; xihat <- t(xihat)/varXs
    sqxiXs <- sqrt(thetaXs)/varXs; sqxiYs <- sqrt(thetaYs)/varYs
    that <- (sqrt(xihat) + abs(corhat)/2*(matrix(sqxiXs, pX, pY) + 
                                        t(matrix(sqxiYs, pY, pX)))) / sqrt(n)
    ## Implementation by Zhang: 
    ##that <- sqrt(xihat/n) + corhat/2*(matrix(diag(thetaXs/varXs/varXs)/n, pX, pY) + t(matrix(diag(thetaYs/varYs/varYs)/n, pY, pX))) ####
  }
  return(list("cor"=corhat,"theta"=thetahat,"xi"=xihat,"t"=that))
}

adp.lasso.thresh <- function(X, lambda){ # Adaptive Lasso thresholding function
  t = 1 - (abs(lambda)/(abs(X)+1e-10))^4
  Z = X * (t > 0) * t
  return(Z)
}

hard.thresh <- function(X, lambda){ # Hard tresholding function
  Z = X * (sign(abs(X) - abs(lambda)) > 0)
  return(Z)
}

adp.D.hat <- function(X1, X2, Y1, Y2, delta){ # adaptive method to calculate differential correlation D-hat with delta specified
  n1 <- dim(X1)[1]; n2 <- dim(X2)[1]; pX <- ncol(X1); 
  if (is.null(Y1)) {pY <- 0}
  else {pY <- ncol(Y1)}
  sam1 <- sample.cor_theta_xi_t(X1,Y1); sam2 <- sample.cor_theta_xi_t(X2,Y2)
  sample_D <- sam1$cor - sam2$cor
  t <- sam1$t + sam2$t
  t <- delta*sqrt(log(pX+pY))*t ### pX+pY for p
  #D_hat <- sample_D * (sign(abs(sample_D) - abs(t)) > 0)   ## hard thresholding  
  D_hat <- adp.lasso.thresh(sample_D, t)   ## Adaptive-Lasso thresholding
  return(D_hat)
}

Cai <- function(X1, X2, Y1=NULL, Y2=NULL, dmax=10, hmax=5, fold=5, verbose=TRUE, seed=NULL){
  set.seed(seed)
  n1 <- nrow(X1); n2 <- nrow(X2)
  ## Cross-validation
  loss_cross_fro <- rep(0, 5*dmax+1)
  for (d in 1:(5*dmax+1)){ # Apply cross-validation to select delta
    loss_cross_fro[d] <- 0
  }
  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = (5*dmax+1)*hmax, style = 3); count <- 1
  }
  for (h in 1:hmax){
    #set.seed(h)
    I1 <- sample.int(n1, round(n1*(1-1/fold))); I2 <- sample.int(n2, round(n2*(1-1/fold)));
    X1_train <- X1[I1,]; X2_train <- X2[I2,]
    NI1 <- setdiff(1:n1, I1); NI2 <- setdiff(1:n2, I2)
    X1_test = X1[NI1,]; X2_test = X2[NI2,];
    if (is.null(Y1)){
      Y1_test <- Y2_test <- Y1_train <- Y2_train <- NULL
    } else {
      Y1_train <- Y1[I1,]; Y2_train <- Y2[I2,]
      Y1_test <- Y1[NI1,]; Y2_test <- Y2[NI2,]
    }
    samp_diff <- (sample.cor(X1_test,Y1_test) - sample.cor(X2_test,Y2_test));
    for (d in 1:(5*dmax+1)){
      #if (verbose) print(paste("h: ",h,"/",hmax,", d: ",d,"/",5*dmax+1,sep=""))
      delta <- (d-1)/dmax
      D_hat <- adp.D.hat(X1_train, X2_train, Y1_train, Y2_train, delta)  # Direct estimate of the difference
      loss_cross_fro[d] <- loss_cross_fro[d] + norm(D_hat - samp_diff, "F")^2
      if (verbose){
        utils::setTxtProgressBar(pb, count); count <- count + 1
      }
    }
  }
  d <- which.min(loss_cross_fro)
  delta_ast <- (d-1)/dmax
  D_hat_ast <- adp.D.hat(X1, X2, Y1, Y2, delta_ast)
  if (verbose) cat("\n") ## After the progressbar ends
  return (D_hat_ast)
}
