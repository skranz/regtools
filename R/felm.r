predict.felm = function(object, newdata, use.fe = TRUE,...) {
  restore.point("predict.felm")  
  co = coef(object)
  
  rownames(newdata) = seq_along(newdata[,1])
  # model matrix without intercept
  mm = model.matrix(object = object,data = newdata)
  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1]
  }
  y.pred = mm %*% co
  
  fe.vars = names(object$fe)
  if (use.fe & length(fe.var)>0) {
    if (!all(fe.vars %in% colnames(mm))) {
      missing = setdiff(fe.vars, colnames(mm))
      warning("Cannot use fixed effects for ", paste0(missing, collapse=", "), " in prediction since no data is given.")
      fe.vars = setdiff(fe.vars, missing)
    }

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
  y.pred
}