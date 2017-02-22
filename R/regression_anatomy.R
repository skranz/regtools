
regression.anatomy.x.tilde = function(mod,X=model.matrix(mod), add.mean = TRUE) {
  xdat = as.matrix(X)
  if (all(xdat[,1]==1)) xdat=xdat[,-1]
  
  x.tilde = xdat
  
  #dat = get.regression.data(mod)
  #xdat = as.matrix(dat[,-1])
  xvars = colnames(xdat)
  
  i = 1
  for (i in seq_along(xvars)) {
    x.tilde[,i] = resid(lm.fit(y = xdat[,i],x = cbind(1,xdat[,-i]))) + ifelse(add.mean,mean(xdat[,i]))
  }
  as.data.frame(x.tilde)
}


#' Regression anatomy plots
#' 
#' Show original scatterplots and scatterplots
#' of y and controlled x_i, i.e. the residuals
#' of a regression of x_i on the other explanatory
#' variables. See e.g. Filoso (2010):
#' https://mpra.ub.uni-muenchen.de/42716/1/FILOSO-Regression_Anatomy_Revealed.pdf

#' @param mod A regression model, a result of a call to lm.
#' @param vars which x variables shall be included in the plot
#' @export
regression.anatomy.plots = function(mod, vars=NULL) {
  dat = get.regression.data(mod)
  xvars = colnames(dat)[-1]
  x.tilde = regression.anatomy.x.tilde(mod)
  tilde = cbind(dat[[1]],x.tilde)
  colnames(tilde) = colnames(dat)
  
  dat$TRANSFORM = "orginal"
  tilde$TRANSFORM = "controlled"
  
  yvar = colnames(dat)[1]
  d = rbind(dat, tilde)
  library(tidyr)
  
  dl = gather_(d,key_col="xvar",value_col="x", gather_cols=xvars)

  if (!is.null(vars)) {
    dl = filter(dl, xvar %in% vars)
  }
    
  library(ggplot2)
  ggplot(data=dl, aes_string(y=yvar,x="x",color="xvar")) +
    geom_point()+
    geom_smooth(method = lm,se = FALSE, color="black")+
    facet_grid(TRANSFORM~xvar,scales = "free_x")

}
