examples.expand.formula.and.data = function() {
  T = 10
  x1 = sample(1:4, T, replace = TRUE)
  x2 = runif(T)
  
  y = x1^2 + log(x2) + rnorm(T)
  x1f = factor(x1)
  
  formula = y~x1f+log(x2) + x1*x2 + I(x1^2)
  terms(formula)$variables[[1]]
  formula.dep.var(formula)
  formula.indep.var(formula)
  
  dat = regression.data(formula)
  regression.formula(dat)
}

#' Returns the dependent variable of a formula object
#' @export
formula.dep.var = function(formula) {
  all.vars(formula[[2]])
}

#' Creates a data.frame with all dummy variables and transformed variables described in formula
#' 
#' @param formula a regression formula e.g. y~log(x1)+x2
#' @param data a data frame that contains original variables used in formula
#' @param normalize.names if TRUE (default) change column names to be valid R variable names
#' @param remove.intercept if TRUE (default) do not generate an .Intercept column
#' 
#' @return a data.frame
#' @export
regression.data = function(formula, data=parent.frame(), normalize.names=TRUE, remove.intercept=TRUE) {
  dep.var = data[[formula.dep.var(formula)]]
  dat = cbind(dep.var, as.data.frame(model.matrix(formula,data)))
  if (remove.intercept) {
    if (all(dat[,2]==1))
      dat = dat[,-2]
  }
  if (normalize.names)
    names = normalize.column.names(colnames(dat))
  colnames(dat)=names
  dat  
} 

#' generates a regression formula from a data.frame assuming the first column is regressed on all other columns
#' @export
regression.formula = function(data, lhs=colnames(data)[1], rhs=setdiff(colnames(data),lhs)) {
  str = paste0(lhs,"~",paste0(rhs,collapse="+"))
  as.formula(str)
}

normalize.column.names = function(names) {
  names = gsub("(","_",names,fixed=TRUE)
  names = gsub(":",".",names,fixed=TRUE)
  names = gsub("*:",".",names,fixed=TRUE)
  names = gsub(")","",names,fixed=TRUE)
  names = gsub("^","_power_",names,fixed=TRUE)
  names
}