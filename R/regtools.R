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

#' Creates a data.frame that can be used to run a regression with the given formula
#' 
#' @param formula a regression formula e.g. y~log(x1)+x2
#' @param reg optionally a regression object from which the formula is takes
#' @param data a data frame that contains original variables used in formula
#' @param normalize.names if TRUE (default) change column names to be valid R variable names
#' @param remove.intercept if TRUE (default) do not generate an .Intercept column
#' @param expand if TRUE, call expanded.regression.data instead
#' 
#' @return a data.frame
#' @export
regression.data = function(formula=formula(reg), reg=NULL, data=parent.frame(), normalize.names=TRUE, remove.intercept=TRUE, expand=FALSE) {
  if (expand) {
    return(expanded.regression.data(formula,reg,data,normalize.names, remove.intercept))
  }  
  dep.var = data[[formula.dep.var(formula)]]
  dat = cbind(dep.var,model.frame(formula,data=data))
  if (normalize.names)
    names = normalize.column.names(colnames(dat))
  colnames(dat)=names
  dat  
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
expanded.regression.data = function(formula, data=parent.frame(), normalize.names=TRUE, remove.intercept=TRUE) {
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


#' Normalize column names such that they are valid variable names in R
#' @export
normalize.column.names <- function(names) {
  names = gsub("-","_",names,fixed=TRUE)
  names = gsub(" ","_",names,fixed=TRUE)
  names = gsub("(","_",names,fixed=TRUE)
  names = gsub(":",".",names,fixed=TRUE)
  names = gsub("*:",".",names,fixed=TRUE)
  names = gsub(")","",names,fixed=TRUE)
  names = gsub("^","_power_",names,fixed=TRUE)
  names
}

#' Rerun the regression reg with a formula and data in which all factors and transformations are expanded to separate (dummy-)variables.
#' @param reg the original regression object (needs to have a fields call, formula and data)
#' @export
expanded.regression = function(reg, org.formula=formula(reg), org.data=get.regression.data(reg), parent.env = parent.frame()) {
  restore.point("expanded.regression")
  substitute.call = function (x, env) {
    call <- substitute(substitute(x, env), list(x = x))
    eval(call)
  }
  
  env = new.env(parent=parent.env)
  edat = expanded.regression.data(org.formula,org.data)
  colnames(edat) = normalize.column.names(colnames(edat))
  eformula = regression.formula(edat)
  
  call = reg$call
  call$formula = eformula
  call$data = as.name("edat")

  env$edat = edat
  eval(call,env)
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

