
examples.effectplot = function() {
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
  effectplot(reg,data,main="Effects")
  
  sdata = scale.data.cols(data,"10-90")
  sreg = glm(y~x + x^2 + z +q, data=sdata, family="binomial")
  summary(sreg)
  coefplot(sreg,sdata)

 }

#' Change units of columns in a data frame, e.g. measure relative to one standard deviation
#' 
#' useful to make coefficients in regressions better comparable
scale.data.cols = function(dat,unit="sd",cols = setdiff(colnames(dat),exclude), exclude=NULL, dummy01=TRUE) {
  
  units = lapply(colnames(dat), function(col) {
    list(unit = "original",size=1, descr="") 
  })
  dummy.unit = list(unit="dummy",size=1,descr="unit: dummy")    
    
    
  unit.val = rep
  names(units)=cols
  for (col in cols) {
    val = dat[[col]]
    if (is.dummy.val(val, dummy01)) {
      attr(dat[[col]],"unit") = dummy.unit
      next
    }
    nval = scale.values(val, unit=unit, var.name=col)
    dat[[col]] = nval
  }
  attr(dat,"units") = units
  dat
}

is.dummy.val = function(val, dummy01=TRUE) {
  if (!is.numeric(val)) {
    return(TRUE)
  } 
  if (dummy01) {
    if (all(unique(val) %in% c(0,1,NA))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

make.val.unit = function(val, unit.code=c("sd","1sd","2sd","4sd","6sd","10-90","20-80","25-75","min-max","dummy", "original")) {

    #unit = "10-90"; val = dat$account_days
  if (unit.code=="original") {
    return(list(code = "original",size=1, descr="")) 
  }  
  val = na.omit(val)
  unit.code = unit.code[1]
  if (unit.code=="sd")
    unit.code="1sd"
  
  if (substring(unit.code,2)=="sd") {
    denom = sd(val) * as.numeric(substring(unit.code,1,1))
  } else if (unit.code=="min-max") {
    denom = max(val)-min(val)
  } else if (unit.code=="dummy") {
    denom = 1
  } else {
    start = as.numeric(substring(unit.code,1,2))
    end = as.numeric(substring(unit.code,4,5))
    denom = diff(quantile(val, c(start, end)/100))
  }
  list(code=unit.code, size=denom, descr=paste0("unit ", unit.code, ": ", signif(denom,3)))
}


scale.values = function(val, 
    unit.code=c("sd","1sd","2sd","4sd","6sd","10-90","20-80","25-75","min-max","nochange"),
    var.name=""
) {
  restore.point("scale.values")
  unit = make.val.unit(val, unit.code)
  ret = val / unit$size
  attr(ret,"unit") = unit
  ret
}

add.values.for.effect = function(val, effect) {
  restore.point("add.values.for.effect")
  val = na.omit(val)
  if (effect$type=="quantile") {
    effect$baseline.val = median(val)
    effect$low.val = quantile(val,effect$low.percent)
    effect$high.val = quantile(val,effect$high.percent)
  } else if (effect$type == "sd") {
    m = mean(val)
    s = sd(val)
    effect$baseline.val = m
    effect$low.val = m-s*0.5*effect$size
    effect$high.val = m+s*0.5*effect$size  
  } else if (effect$type == "dummy") {
    effect$baseline.val = median(val)
    effect$low.val = 0
    effect$high.val = 1  
  # original values
  } else {
    m = mean(val)
    effect$baseline.val = m
    effect$low.val = m-0.5*effect$size
    effect$high.val = m+0.5*effect$size  
  }
  effect
}


get.effect.base = function(val=NULL, effect=c("sd","1sd","2sd","4sd","6sd","10-90","20-80","25-75","min-max","dummy", "original"), var=NULL) {
  
  if (is.character(effect)) {
    code= effect[1]
    if (code=="sd")
      code="1sd"

    if (code=="original") {
      effect=list(code = "original",type="original",var=var, descr="") 
    } else if (substring(code,2)=="sd") {
      effect=list(code = code,type="sd",var=var, size= as.numeric(substring(code,1,1)))
    } else if (code=="dummy") {
      effect = list(code=code, type=code)
    } else {
      start = as.numeric(substring(code,1,2))
      end = as.numeric(substring(code,4,5))
      effect = list(code = code,type="quantile",var=var, low.percent=start/100, high.percent=end/100)
    }
  }
  if (!is.null(val))
    effect=add.values.for.effect(val, effect)
  
  effect
}


get.effect.base.df = function(dat, numeric.effect = "10-90", dummy01 = TRUE) {
  restore.point("get.effect.base.df")
  
  dummy.effect = list(type="dummy")
  val = dat[[1]]
  li =  lapply(colnames(dat), function(col) {
    val = dat[[col]]
    if (is.dummy.val(val)) {
      effect= get.effect.base(val, "dummy", var=col)
    } else {
      effect = get.effect.base(val, numeric.effect, var=col)
    }
    effect = add.values.for.effect(val,effect)
    as.data.frame(c(list(var=col,code=effect$code, type=effect$type),
      effect[c("baseline.val","low.val","high.val")]
    ))
  })
  df = do.call(rbind,li)
  rownames(df)=NULL
  #df = rbindlist(li)
  df$val.descr = paste0(signif(df$low.val,3)," ",signif(df$baseline.val,3)," ",signif(df$high.val,3))
  df
}

#' Simple function to compute effect sizes used by effectplot
#' @export
get.effect.sizes = function(reg, dat,
    vars=intersect(colnames(dat), names(coef(reg))),
    scale.depvar=NULL, depvar = names(reg$model)[[1]],data.fun = NULL,numeric.effect="10-90", dummy01=TRUE, predict.type="response") {
  
  restore.point("get.effect.sizes")
  
  library(tidyr)
  
  ebd = get.effect.base.df(dat, numeric.effect=numeric.effect, dummy01=dummy01)
  
  nr = length(vars)*2
  base.df = as.data.frame(t(ebd$baseline.val))
  colnames(base.df) = colnames(dat)
  base.df
  
  key.df = expand.grid(list(level=c("low","high"),var=vars))
  df = cbind(key.df,base.df)
  
  var.ebd = match(vars,ebd$var)
  names(var.ebd) = vars

  row = 0
  for (var in vars) {
    row = row+1
    df[row, var] = ebd$low.val[var.ebd[var]]
    row = row+1
    df[row, var] = ebd$high.val[var.ebd[var]]
  }
  df

  # compute values of dependent data like 
  # df$x_sqr = df$x^2
  if (!is.null(data.fun)) {
    df = data.fun(df)
  }
  
  pred = predict(reg,newdata=df, type=predict.type)
  if (!is.null(scale.depvar)) {
    size = make.val.unit(dat[[depvar]], scale.depvar)$size
    pred = pred/ size 
  }

  
  rdf = cbind(key.df, pred)
  

  d = spread(rdf, key = level, value=pred)
  d$effect = d$high-d$low
  d$abs.effect = abs(d$effect)
  d$effect.sign = sign(d$effect) 
  d$base.descr = ebd$val.descr[var.ebd]
  d
}


#' Plot for regressions to compare effects sizes of normalized changes in the explanatory variables
#' 
#' The plot shall help to compare magnitudes of the influence of different explanatory variables. The default effect is "10-90", i.e. the effect of when -ceteris paribus- changing an (numeric) explanatory variable from its 10% quantile value to its 90% quantile. For dummy variables, we just consider the effect from changing it from 0 to 1.
#' 
#' @param reg the results from a regression, e.g. from a call to lm or glm
#' @param dat the data frame the regression was estimated from
#' @param vars the explanatory variables that shall be shown in the regression lplot
#' @param numeric.effect a code describing the lowest and highest values of numeric explanatory variables used to calculate the effect, e.g. "05-95" means taking the effect of moving from the 5% to the 95% quantile.
#' @param dummy01 shall numeric varibles that have only 0 and 1 as values be treated as a dummy variables?
#' @param sort if TRUE (default) sort the effects by size
#' @param scale.depvar
#' @param depvar name of the dependent variable  
#' @export
effectplot = function(reg, dat,
  vars=intersect(colnames(dat), names(coef(reg))),
  numeric.effect="10-90", dummy01=TRUE,
  sort = TRUE,
  scale.depvar=NULL, depvar = names(reg$model)[[1]],
  xlab="Explanatory variables\n(low baseline high)", ylab=paste0("Effect on ", depvar,""), colors = c("pos" = "#11AAAA", "neg" = "#EE3355"),...
) {
  library(ggplot2)
  
  es = get.effect.sizes(reg,dat, vars,depvar=depvar, scale.depvar=scale.depvar,numeric.effect=numeric.effect, dummy01=dummy01)
  
  restore.point("effectplot")
  
  es$coef.name = paste0(es$var,"\n",es$base.descr)
  if (sort)
    es = es[order(es$abs.effect), ]
 
  es$sign = ifelse(sign(es$effect)>=0,"pos","neg")
  # Set factor name in order to show sorted plot
  es$name = factor(es$coef.name, es$coef.name)


  #qplot(data=es, y=abs.effect, x=name, fill=sign, geom="bar", stat="identity",xlab=xlab,ylab=ylab) +  coord_flip() +

  qplot(data=es, y=abs.effect, x=name, fill=sign, geom="bar", stat="identity",xlab=xlab,ylab=ylab,...) +  coord_flip() +  scale_fill_manual(values=colors) #+ theme_wsj()

}

#' Graphically compare sizes of regression coefficient
#' 
#' mainly useful if regression is run on data that has been normalized by set.data.units
#' @export
coefplot = function(reg,data=NULL, sort=TRUE, remove.intercept=TRUE, show.units=!is.null(data)) {
  library(ggplot2)
  
  restore.point("coef.plot")
  coef = coef(reg)
  if (remove.intercept)
    coef = coef[-1]
  
 
  df = data.frame(coef.name=names(coef),coef=coef, abs.coef = abs(coef), sign=ifelse(sign(coef)>=0,"pos","neg"), stringsAsFactors=FALSE)
  
  if (show.units) {
    cols = intersect(names(coef),colnames(data))
    units = lapply(df$coef.name, function(col) {
      if (col %in% cols)
        return(attr(data[[col]],"unit"))
      return(list(unit = "original",size=1, descr=""))
    })
    units.descr = lapply(units, function(unit) unit$descr)
    df$coef.name = paste0(df$coef.name,"\n",units.descr)
  }

  if (sort)
    df = df[order(abs(coef)), ]
 
  # Set factor name in order to show sorted plot
  df$name = factor(df$coef.name, df$coef.name)

  qplot(data=df, y=abs.coef, x=name, fill=sign, geom="bar", stat="identity") +  coord_flip() #+ theme_wsj()
}

