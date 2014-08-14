#' Get clustered robust standard errors for regression model mod
#' 
#' R-codes (www.r-project.org) for computing clustered-standard errors from Mahmood Arai, Jan 26, 2008.
#' 
#' @param cluster1 variable name of cluster1 
#' @param cluster2 variable name of cluster2
vcovCluster <- function(mod, cluster1, cluster2=NULL, dfcw=1) {
  restore.point("vcovCluster")
  if (is.null(cluster2)) {
    vcovClusterOneWay(mod, cluster1, dfcw)
  } else {
    vcovClusterTwoWays(mod, cluster1,cluster2, dfcw)    
  }
  
}

vcovClusterOneWay <- function(fm, cluster, dfcw=1){
  # R-codes (www.r-project.org) for computing
  # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
  
  # The arguments of the function are:
  # fitted model, cluster1 and cluster2
  # You need to install libraries `sandwich' and `lmtest'
  
  # reweighting the var-cov matrix for the within model
  restore.point("vcovClusterOneWays")
  library(sandwich);library(lmtest)

  cluster = get.regression.var(fm, cluster)


  
  M <- length(unique(cluster))   
  N <- length(cluster)           
  K <- fm$rank                        
  dfc <- (M/(M-1))*((N-1)/(N-K))  
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
  vcovCL
}

vcovClusterTwoWays <- function(fm, cluster1, cluster2, dfcw=1){
   # R-codes (www.r-project.org) for computing multi-way 
   # clustered-standard errors. Mahmood Arai, Jan 26, 2008. 
   # See: Thompson (2006), Cameron, Gelbach and Miller (2006)
   # and Petersen (2006).
# reweighting the var-cov matrix for the within model
   
   # The arguments of the function are:
   # fitted model, cluster1 and cluster2
   # You need to install libraries `sandwich' and `lmtest'
   
   restore.point("vcovClusterTwoWays")
   library(sandwich);library(lmtest)
   cluster1 = get.regression.var(fm, cluster1)
   cluster2 = get.regression.var(fm, cluster2)

   
   cluster12 = paste(cluster1,cluster2, sep="_�_")
   M1  <- length(unique(cluster1))
   M2  <- length(unique(cluster2))   
   M12 <- length(unique(cluster12))
   N   <- length(cluster1)          
   K   <- fm$rank             
   dfc1  <- (M1/(M1-1))*((N-1)/(N-K))  
   dfc2  <- (M2/(M2-1))*((N-1)/(N-K))  
   dfc12 <- (M12/(M12-1))*((N-1)/(N-K))  
   u1j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster1,  sum)) 
   u2j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster2,  sum)) 
   u12j  <- apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum)) 
   vc1   <-  dfc1*sandwich(fm, meat=crossprod(u1j)/N )
   vc2   <-  dfc2*sandwich(fm, meat=crossprod(u2j)/N )
   vc12  <- dfc12*sandwich(fm, meat=crossprod(u12j)/N)
   vcovMCL <- (vc1 + vc2 - vc12)*dfcw
   vcovMCL
}

#' Show a model summary with robust standard errors
#'  
#' Using the sandwich package, creates summary with heteroscedasticity consistent standard errors (Stata�s robust dafault (Stata (2014)))
#' @export
robust.se.summary = function(m, type = "HC3",...){
  vcov = vcovRobust(m, type,...)
  coeftest(m,vcov)
}

#' Wrapper to get robust vcov of different kinds
#'  
#' @export
vcovRobust = function(model, type="HC3", cluster1=NULL,cluster2=NULL,...) {
  if (type=="cluster1") {
    vcovCluster(model, cluster1, cluster2=NULL,...)    
  } else if (type=="cluster" | type=="cluster2") {
    vcovCluster(model, cluster1, cluster2,...)  
  } else if (type=="HAC") {
    vcovHAC(model,...)
  } else {
    vcovHC(model, type,...)
  }
}

#' Extract data from a regression model
#' 
#' First looks whether the model mod has a field 'custom.data' in which a user can manually store the data.frame. 
#' 
#' @export
get.regression.data = function(mod) {
  restore.point("get.regression.data")

  if (!is.null(mod$custom.data))
    return(mod$custom.data)
  
  if (!is.null(mod$data))
    return(mod$data)
  # Model frame
  return(mod$model)
}

#' Extract values of the variable var from a regression model mod
#' 
#' Reports an error if the data for that variable cannot get extracted 
#' from the regression model mod. 
#' 
#' @export
get.regression.var = function(mod, var, as.data.frame = length(var)>1) {
  restore.point("get.regression.var")
  
  data = get.regression.data(mod)
  
  has.var = var %in% names(data)
  if (!all(has.var)) {
    missing.var = paste0(var[!has.var], collapse=", ")
    stop("I could not find the variable(s) ", missing.var, " in your regression model. Try to add a data frame with that variable as field 'custom.data' to your model object.")
  }
  if (as.data.frame | length(var)>1)
    return(as.data.frame(data[var]))
  
  data[[var]]
}
