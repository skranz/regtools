examples.bootstrap.predict.glm = function() {
  # simulate some data
  set.seed(12345)
  n = 1000
  x = rnorm(n)
  z = rnorm(n)
  q = rnorm(n)
  
  # binary outcome
  y = ifelse(pnorm(1 + 0.5*x + 0.25*x^2 - 0.5*z + rnorm(n))>0.5, 1, 0)

  data = data.frame(y,x,z,q)
  # Logit regression
  reg = glm(y~x + x^2 + z +q, data=data, family="binomial")
  effectplot(reg,data,main="Effects", horizontal=TRUE, show.ci=TRUE)

  newdata = data[1:2,-1]
  bootstrap.predict.glm(reg, newdata, repl=100)
  boot
}  

bootstrap.predict.glm = function(reg, newdata,repl=100, addintercept=TRUE, coef.li=NULL) {
  newmatrix = as.matrix(newdata)
  if (addintercept)
    newmatrix<-cbind(1,newmatrix)

  y = reg$model[,1]
  x = reg$model[,-1]
  if (addintercept)
    x = cbind(intercept=1,x)
  
  #coef.mat = draw.from.estimator(n=1000,coef=reg$coef, vcov=vcov(reg))
  coef.li = replicate(repl,one.bootstrap.coef.glm(reg, x=x,y=y), simplify=FALSE)
  pred.li = lapply(coef.li,predict.from.coef.glm,newmatrix=newmatrix, reg=reg )
  pred.li  
}

draw.from.estimator = function(n=1,coef=reg$coef, vcov=vcov(reg)) {
  #restore.point("draw.from.estimator")
  library(mvtnorm)
  rmvnorm(n,mean=coef,sigma=vcov)
} 

one.bootstrap.coef.glm =function(reg, x=NULL,y=NULL,rows = NULL) {
  restore.point("one.bootstrap.prediction.glm")
  if (is.null(y))
    y = reg$model[,1]  
  if (is.null(x))
    x = cbind(1,reg$model[,-1])
  
  if (is.null(rows)) {
    n = NROW(odat)
    rows = sample.int(n, replace=TRUE)
  }
  ny = y[rows]
  nx = x[rows,]
   
  glm.fit(x=nx,y=ny, family=reg$family)$coef
}

get.predict.from.coef.fun = function(reg, ...) {
  # Manual dispatch
  if (is(reg,"glm"))
    return(predict.from.coef.glm)
  if (is(reg,"lm"))
    return(predict.from.coef.lm)
  
  return(NULL)
  #stop("We have no predict from coef fun for regression models of class ", class(reg)[1], " yet.")
}


predict.from.coef.glm = function(coef=reg$coef,newmatrix,reg=NULL, linkinv=reg$family$linkinv,...) {
  eta<- as.numeric(newmatrix %*% coef)
  reg$family$linkinv(eta)
}

predict.from.coef.lm = function(coef=reg$coef,newmatrix,reg=NULL) {
  as.numeric(newmatrix %*% coef)
}
