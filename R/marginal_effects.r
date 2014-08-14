
#' Wrapper to mfx package to extract marginal effects from a glm model
#' 
#' not yet all possible types of glm models implemented, need to extend
#' 
glm.marginal.effects = function(mod, mfx.fun=NULL,...) {
  library(mfx)
  
  if (is.null(mfx.fun)) {
    family = mod$family$family
    link = mod$family$link
    if (link=="probit") {
      mfx.fun = probitmfx
    } else if (link=="logit") {
      mfx.fun = logitmfx
    } else if (family=="poisson") {
      mfx.fun = poissonmfx
    } else {
      stop("could not find mfx.fun for your model!")
    }
  }
  formula = mod$formula
  data = mod$dat
  
  res = mfx.fun(formula, data,...)
  res
}
