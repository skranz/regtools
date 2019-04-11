examples.predict.felm = function() {
  library(regtools)
  
  T = 12
  x <- 1:T
  fe1 = sample(c("yes","no"), T, replace=TRUE)
  fe2 = sample(c("a","b","c"), T, replace=TRUE)
  y <- x  + (fe1=="yes") + 2*(fe2=="a") + (fe2=="b")+ rnorm(T)
  
  new <- data.frame(
    x = (T+1):(2*T),
    fe1 = sample(c("yes","no"), T, replace=TRUE),
    fe2 = sample(c("a","b","c"), T, replace=TRUE)    
  )
  p.lm = predict(lm(y~x+log(x)+fe1+fe2), new)
  
  library(lfe)
  felm = felm(y~x+log(x)+fe1|fe2)
  p.felm = predict.felm(felm,new)
  
  # Compare lm and felm predictions
  # the predicted values should be the same
  cbind(p.lm, p.felm)
  
   library(regtools)
  
  T = 12
  x <- 1:T
  fe1 = sample(c("yes","no"), T, replace=TRUE)
  fe2 = sample(c("a","b","c"), T, replace=TRUE)
  y <- x  + (fe1=="yes") + 2*(fe2=="a") + (fe2=="b")+ rnorm(T)
  
  new <- data.frame(
    x = (T+1):(2*T),
    fe1 = sample(c("yes","no"), T, replace=TRUE),
    fe2 = sample(c("a","b","c"), T, replace=TRUE)    
  )
  p.lm = predict(lm(y~x+log(x)+fe1+fe2), new)
  
  library(lfe)
  felm = felm(y~x+log(x)+fe1+fe2)
  p.felm = predict.felm(felm,new)
  
  # Compare lm and felm predictions
  # the predicted values should be the same
  cbind(p.lm, p.felm)
}

#' An implementaion of predict for felm
predict.felm = function(object, newdata, use.fe = TRUE,...) {
  restore.point("predict.felm")
  co = coef(object)
  
  # too make code compatible with tibbles
  newdata = as.data.frame(newdata)
  
  rownames(newdata) = seq_along(newdata[,1])
  
  form = formula(object)
  # Need to extract part before first |
  # use brute force string manipulation
  #library(Formula) # will be loaded by lfe anyways
  #form.str = as.character(Formula(form))
  
  form.str = capture.output(form)
  pos = regexpr("|", form.str, fixed=TRUE)
  if (pos > 0) {
    form.str = substr(form.str,1,pos-1)
    form = as.formula(form.str)
  }
  
  
  # model matrix
  mf = model.frame(form, newdata)
  mm = model.matrix(form,data=mf)
  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept if not included in coefficients
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1,drop=FALSE]
  }
  y.pred = mm %*% co
  
  fe.vars = names(object$fe)
  if (use.fe & length(fe.vars)>0) {
    rows = as.integer(rownames(mm))
    nd = newdata[rows,]
    all.fe = getfe(object)
    fe.var = fe.vars[1]
    for (fe.var in fe.vars) {
      df = all.fe[all.fe$fe == fe.var,]
      frows = match(nd[[fe.var]],df$idx)    
      myfe = df$effect[frows]
      myfe[is.na(myfe)] = 0
      
      y.pred = y.pred + myfe
    }
  }
  as.vector(y.pred)
}