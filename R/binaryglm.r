#' Authors: Joachim Plath and Sebastian Kranz
# 
# is.perfect.predictor = function(y,x) {
#   restore.point("is.perfect.predictor")
#   x0 = x[y==0]
#   x1 = x[y==1]
#   
#   if (length(x0)==0 | length(x1)==0)
#     return(FALSE)
# 
#   # can be perfectly separated
#   max(x0) < min(x1) | max(x1) < min(x0) 
# }
# 
# get.perfect.predictor.cols = function(reg.data, max.unique=Inf) {
#   restore.point("get.perfect.predictor.cols")
#   reg.data = na.omit(reg.data)
#   X =as.data.frame(reg.data)
#   num.unique = sapply(X,unique)
#   cols = setdiff(which(num.unique<=max.unique),1)
# 
#   is.pp = sapply(col, function(col) {
#     is.perfect.predictor(y=X[,1],x=X[,col])
#   })
#   colnames(reg.data)[cols[is.pp]]
#   
# }
# 
# probit = function(formula, data, drop.perfect.predictors=FALSE,show.drops=TRUE,...) {
#   my.binary.glm(formula,data,link="probit",drop.perfect.predictors,show.drops,...)
# }
# 
# my.binary.glm=function(formula,data,link="probit",drop.perfect.predictors=FALSE,show.drops=TRUE,...) {
#   restore.point("my.binary.glm")
#   
#   
#   if (!drop.perfect.predictors) {
#     glm(f,data=X,family=binomial(link=link),...)
#   }
#   if(class(formula) != "formula") {
#     stop("formula must be of type formula")
#   }
#     
#   if(!link %in% c("probit","logit")) {
#     stop("Link must be 'logit' or 'probit'")
#   }
# 
#   X = regression.data(formula, data=data, remove.intercept=TRUE)
#   pp = get.perfect.predictor.cols(X, max.unique=max.unique)  
#   
#   
#   cols = setdiff(colnames(X,pp))
#   if(show.drops & length(pp)>0) {
#     cat("\ndropped variables due to perfect prediction:\n",
#         paste0(pp, collapse=", ")
#         "\n")
#   }
#   X = X[,cols]  
#   f= regression.formula(X)
#   glm(f,data=X,family=binomial(link=link),...)
#   
# }