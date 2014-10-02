
examples.showreg = function() {
  

  # iv and ols with robust standard errors
  library(AER)
  data("CigarettesSW", package = "AER")
  CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

  iv <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),data = CigarettesSW, subset = year == "1995")
  ols <- lm(log(packs) ~ log(rprice) + log(rincome),data = CigarettesSW, subset = year == "1995")

  showreg(list(iv=iv,iv.rob=iv, ols=ols,  ols.rob=ols),
          robust=c(FALSE,TRUE,FALSE,TRUE), robust.type="HC4")  
  showreg(list(iv=iv,iv.rob=iv, ols=ols,  ols.rob=ols),
          robust=c(FALSE,TRUE,FALSE,TRUE), robust.type="NeweyWest", prewhite=FALSE)  


  # Marginal effect for probit regression
  set.seed(12345)
  n = 1000
  x = rnorm(n)
  
  # binary outcome
  y = ifelse(pnorm(1 + 4*x + rnorm(n))>0.5, 1, 0)
  data = data.frame(y,x)

  reg = glm(y~x, data=data, family=binomial(link=probit))
  showreg(list("probit"=reg,"marginal effects"=reg), coef.transform=c("no", "mfx"), omit.coef = "(Intercept)")

  # Clustered standard errors
  # (not tested for correctness at all)
  library(Ecdat)
  data(Fatality)
  LSDV <- lm(mrall ~ beertax + factor(year) + factor(state), data=Fatality)
  LSDV$custom.data = Fatality
  showreg(
    list("LSDV"=LSDV,
         "LSDV (state cluster)"=LSDV,
         "LSDV (state-year cluster)"=LSDV
    ),
    robust=c(FALSE,TRUE,TRUE),
    robust.type=c("const","cluster1","cluster2"),
    cluster1 = "state", cluster2="year"
  )

}

#' Show and compare regression results
#' 
#' The function extends and wraps the screenreg, texreg and htmlreg functions in the texreg package. It allows for robust standard errors (also clustered robust standard errors) and can show marginal effects in glm models.
#' 
#' @param l list of models as in screenreg
#' @param custom.model.names custom titles for each model
#' @param robust shall robust standard errors be used? Logical single number or a vector specifying for each model.
#' @param robust.type the type of robust standard errors. Can be "HAC", "cluster", "HC1" to "HC4" or "NeweyWest"
#' @param vcov.list optional a list of covariance matrices of the coefficients for every model
#' @param cluster1 and cluster2 if clustered robust standard errors are computed the name of the variables that shall be clustered by
#' @param coef.transform either NULL or a vector containing "no" or "mfx", if an entry is "mfx" we show the marginal effects of the corresponding model. 
#' @param ... additional parameters for screenreg, texreg or htmlreg
#' 
#' @export
showreg = function(l,custom.model.names=names(l), robust = FALSE, robust.type = "HC3", cluster1=NULL, cluster2=NULL,coef.transform = NULL, vcov.li=NULL, coef.mat.li = NULL, digits = 2, output=c("text","html","latex"), output.fun = NULL, doctype = FALSE,...){
  restore.point("showreg")

  if (is.null(output.fun)) {
    output = output[1]
    library(texreg)
    if (output=="text"){
      output.fun=screenreg
    } else if (output=="html") {
      output.fun = htmlreg
    } else if (output=="latex") {
      output.fun = texreg
    }
  }
    
  if (!any(robust) & is.null(vcov.li) & is.null(coef.mat.li) & is.null(coef.transform)) {
    return(output.fun(l, ..., custom.model.names=custom.model.names,digits = digits, doctype=doctype))
  }    
  
  if (length(robust)==1)
    robust = rep(robust,length(l))
  
  if (length(robust.type)==1)
    robust.type = rep(robust.type,length(l))
   
  robust.type[!robust] = "const"
  
  if (is.null(vcov.li)) {
    vcov.li = lapply(seq_along(l), function(i){
      vcovRobust(l[[i]], type = robust.type[[i]], cluster1=cluster1, cluster2=cluster2)
    })
  }
    
  cml = lapply(seq_along(l), function(i){
    get.coef.mat(l[[i]],vcov=vcov.li[[i]], robust = robust[[i]], robust.type = robust.type[[i]], coef.transform = coef.transform[[i]], cluster1 = cluster1[[i]], cluster2 = cluster2[[i]], coef.mat = coef.mat.li[[i]])
  })

  coef.li = lapply(cml, function(r) convert.na(r[,1],Inf))
  se.li = lapply(cml, function(r) convert.na(r[,2],Inf))
  pval.li = lapply(cml, function(r) convert.na(r[,4],1))
 
  
  output.fun(l,..., custom.model.names = custom.model.names,
      override.coef = coef.li, override.se = se.li,
      override.pval = pval.li,
      digits = digits,doctype=doctype)
}

#' get the matrix with coefficients, se, t-value, p-value of a regression model
#' 
#' internally used by showreg
get.coef.mat = function(mod, vcov=NULL, robust = FALSE, robust.type = "HC3", coef.transform = NULL, cluster1=NULL, cluster2=NULL,  coef.mat = NULL) {
  restore.point("get.coef.mat")
  
  # coef.mat is given, so just return it
  if (!is.null(coef.mat))
    return(coef.mat)
  
  # For marginal effects use the functions from mfx
  if (isTRUE(coef.transform=="mfx")) {
    mfx = glm.marginal.effects(mod, robust=robust, clustervar1=cluster1, clustervar2 = cluster2)
    df = as.data.frame(mfx$mfxest)
    
    # Add a missing intercept
    if (NROW(df)<length(coef(mod))) {
      idf = data.frame(NaN,NaN,NaN,NaN)
      colnames(idf) = colnames(df)
      rownames(idf) = names(coef(mod))[1]
      df = rbind(idf,df)
    }  
    return(df)
  }

  # Compute robust vcov
  if (is.null(vcov) & robust) {
    vcov = vcovRobust(l[[i]], type = robust.type, cluster1=cluster1, cluster2=cluster2)
  }
  
  # return the coefficient matrix
  coeftest(mod, vcov.=vcov)
}

convert.na = function(x,new.val) {
  x[is.na(x)] = new.val
  x
}

