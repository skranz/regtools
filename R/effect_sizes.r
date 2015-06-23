
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

add.values.for.effect = function(val, effect, overwrite=TRUE) {
  restore.point("add.values.for.effect")
  val = na.omit(val)
  
  if (!overwrite)
    old.effect = effect
  
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
  if (!overwrite) {
    effect[names(old.effect)] = old.effect
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


get.effect.base.df = function(dat, numeric.effect = "10-90", dummy01 = TRUE, effect.bases=NULL) {
  restore.point("get.effect.base.df")
  
  dummy.effect = list(type="dummy")
  val = dat[[1]]
  li =  lapply(colnames(dat), function(col) {
    val = dat[[col]]
    if (is.null(effect.bases[[col]])) {
      if (is.dummy.val(val)) {
        effect= get.effect.base(val, "dummy", var=col)
      } else {
        effect = get.effect.base(val, numeric.effect, var=col)
      }
    } else {
      effect = add.values.for.effect(val,effect.bases[[col]], overwrite=FALSE)
    } 
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
    scale.depvar=NULL, depvar = names(reg$model)[[1]],data.fun = NULL,numeric.effect="10-90", dummy01=TRUE, predict.type="response", effect.bases=NULL, compute.se=FALSE, ci.prob=0.95, repl.for.se=10000) {
  
  restore.point("get.effect.sizes")
  
  library(tidyr)
  
  ebd = get.effect.base.df(dat, numeric.effect=numeric.effect, dummy01=dummy01, effect.bases=effect.bases)
  
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

  newdata = df[,-(1:3)]
  
  # compute values of dependent data like 
  # df$x_sqr = df$x^2
  if (!is.null(data.fun)) {
    df = data.fun(df)
  }
  
  if (is(reg,"felm")) {
    pred = predict.felm(reg,newdata=newdata, use.fe = FALSE)
  } else {
    pred = predict(reg,newdata=newdata, type=predict.type)
  }
  scale.y  =1
  if (!is.null(scale.depvar)) {
    scale.y = make.val.unit(dat[[depvar]], scale.depvar)$size
    pred = pred / scale.y 
  }
  rdf = cbind(key.df, pred)
  
  d = spread(rdf, key = level, value=pred)
  d$effect = d$high-d$low
  d$abs.effect = abs(d$effect)
  d$effect.sign = sign(d$effect) 
  d$base.descr = ebd$val.descr[var.ebd]
  
  if (compute.se) {
    newdf = model.matrix.from.new.data(newdata,reg)
    se.df = compute.effect.size.se(reg, repl.for.se,newdata=newdf,scale=scale.y)     
    d = cbind(d, se.df)
  }  
  d
}

model.matrix.from.new.data = function(newdata, reg, na.action=na.pass,...) {
  object=reg
  tt <- terms(object)
  Terms <- delete.response(tt)
  na.action=na.pass
  
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = reg$xlevels)
  X <- model.matrix(Terms, m, contrasts.arg = reg$contrasts)
  X
}

compute.effect.size.se = function(reg, repl.for.se,newdata,scale=1, add.intercept = FALSE, ci.prob=c(0.05,0.95),...) {
  restore.point("compute.effect.size.se")
  
  newmatrix = as.matrix(newdata)
  if (add.intercept)
    newmatrix=cbind(intercept=1,newmatrix)
  
  pred.fun = get.predict.from.coef.fun(reg)
  # check if pred.fun is null
  if (is.null(pred.fun)) {
    warning("No predict.from.coef function for model of class ", class(reg)[1], " skip computation of se and confidence intervals for effect.")
    return(NULL)
  }
  
  coef.mat = draw.from.estimator(n=repl.for.se,coef=reg$coef, vcov=vcov(reg))
  
  # P rows of newmatrix (number of predictions)
  # R number of draws from estimator
  # P x R
  mat = apply(coef.mat,1,pred.fun,newmatrix=newmatrix, reg=reg )
  mat[,1:2]
  
  pred.vec = as.numeric(apply(coef.mat,1,pred.fun,newmatrix=newmatrix, reg=reg ))
  pred.mat = matrix(pred.vec,ncol=2, byrow=TRUE)
  pred.effect = (pred.mat[,2]-pred.mat[,1]) / scale

  effect.mat = matrix(pred.effect, nrow=repl.for.se, byrow=TRUE)
  effect.se  = apply(effect.mat,2,sd, na.rm=TRUE)
  if (length(ci.prob)==1)
    ci.prob = c((1-ci.prob)/2, 1- (1-ci.prob)/2)
  ci = apply(effect.mat,2, quantile, probs=ci.prob, na.rm=TRUE)
  
  data.frame(effect.se = effect.se, ci.low=ci[1,], ci.high=ci[2,])
  
}


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
  effectplot(reg,main="Effects", horizontal=TRUE, show.ci=TRUE)


  # An example with factors
  
  T = 10
  x = sample(1:4, T, replace = TRUE)
  y = x^2 + rnorm(T)
  xf = as.character(x)

  # effectplot can currently not handle factor variables
  # in the regression formula
  reg = lm(y~xf)
  effectplot(reg)
  
  # Workaround: first explicitly generate a 
  # data.frame with all dummy variables
  dat = expanded.regression.data(y~xf)
  reg = lm(regression.formula(dat), data=dat)
  reg
  effectplot(reg)

  # Short-cut
  reg = expanded.regression(lm(y~xf))
  effectplot(reg)
  
}


#' Plot for regressions to compare effects sizes of normalized changes in the explanatory variables
#' 
#' The plot shall help to compare magnitudes of the influence of different explanatory variables. The default effect is "10-90", i.e. the effect of when -ceteris paribus- changing an (numeric) explanatory variable from its 10% quantile value to its 90% quantile. For dummy variables, we just consider the effect from changing it from 0 to 1.
#' 
#' @param reg the results from a regression, e.g. from a call to lm or glm
#' @param dat default = the data frame the regression was estimated from
#' @param vars the explanatory variables that shall be shown in the plot
#' @param numeric.effect a code describing the lowest and highest values of numeric explanatory variables used to calculate the effect, e.g. "05-95" means taking the effect of moving from the 5 percent to the 95 percent quantile.
#' @param dummy01 shall numeric varibles that have only 0 and 1 as values be treated as a dummy variables?
#' @param sort if TRUE (default) sort the effects by size
#' @param scale.depvar a scaling for the dependent variable
#' @param depvar name of the dependent variable
#' @param xlab, ylab labels
#' @param colors colors for positive values (pos) and negative values (neg)  
#' @param horizontal shall bars be shown horizontally?
#' @param show.ci shall confidence intervals be shown?
#' @param ci.prob left and right probability level for confidence intervals
#' @param num.ticks the number of tick marks on the effect size axis
#' @param add.numbers shall the effect sizes be added as numbers in the plot?
#' @param numbers.align How shall the effect size numbers be aligned: "left","center", "right" align all numbers at the same horizontal posistion for all variables. "left_of_bar_end" and "right_of_bar_end" align them at the end of each bar.
#' @param numbers.vjust Vertical adjustment of the numbers
#' @param left.margin extra margin left of bars as fraction of maximum bar width
#' @param right.margin extra margin right of bars as fraction of maximum bar width
#' @param signif.digits number of significant digits for effect sizes
#' @param round.digits number of digits effect sizes shall be rounded to
#' @param ... further arguments passed to qplot. E.g. you can set "main" to specify a title of the plot.
#' @export
effectplot = function(reg, dat=get.regression.data(reg,source.data=source.data),source.data = NULL,
  vars=intersect(colnames(dat), names(coef(reg))),
  ignore.vars = NULL,
  numeric.effect="10-90", dummy01=TRUE,
  sort = TRUE,
  scale.depvar=NULL, depvar = names(reg$model)[[1]],
  xlab="Explanatory variables\n(low baseline high)",
  ylab=paste0("Effect on ", depvar,""),
  colors = c("pos" = "#11AAAA", "neg" = "#EE3355"),
  effect.sizes=NULL, effect.bases = NULL, horizontal=TRUE,
  show.ci = FALSE, ci.prob =c(0.05,0.95), num.ticks=NULL, 
  add.numbers=TRUE, 
  numbers.align = c("center","left","right","left_of_bar_end","right_of_bar_end")[1],
  numbers.vjust = ifelse(show.ci,0,0.5),
  left.margin = 0, right.margin=0,
  signif.digits = 2, round.digits=8, 
  alpha = 0.8,...
) {
  library(ggplot2)

# 
#   org.dat = dat
#   
#   if (missing(model.matrix)) {
#     model.matrix = dat
#     try({
#       model.matrix <- model.matrix(reg)
#       if (all(model.matrix[,1]==1))
#         model.matrix <- model.matrix[,-1, drop=FALSE]
#       },silent=TRUE)
#   }
#   if (!is.null(model.matrix)) {
#     dat = cbind(dat[[depvar]],as.data.frame(model.matrix))
#     colnames(dat)[1] = depvar
#   }
#   rownames(dat) = NULL
#   if (missing(vars))
#     vars = colnames(dat)
  
  restore.point("effectplot")

  vars = setdiff(vars, ignore.vars)  
  if (is.null(effect.sizes)) {
    es = get.effect.sizes(reg,dat, vars,depvar=depvar, scale.depvar=scale.depvar,numeric.effect=numeric.effect, dummy01=dummy01, effect.bases=effect.bases, compute.se=show.ci, ci.prob=ci.prob)
  } else {
    es = effect.sizes
  }
  
  
  
  
  es$coef.name = paste0(es$var,"\n",es$base.descr)
  if (sort)
    es = es[order(es$abs.effect), ]
 
  es$sign = ifelse(sign(es$effect)>=0,"pos","neg")
  # Set factor name in order to show sorted plot
  es$name = factor(es$coef.name, es$coef.name)

  es$round.effect = round(es$effect,round.digits)
  es$round.effect = sapply(es$round.effect, signif, digits=signif.digits)

  add.str = ifelse(es$round.effect>=0, " ","")
  es$round.effect = paste0(add.str,es$round.effect)
  
  

  #qplot(data=es, y=abs.effect, x=name, fill=sign, geom="bar", stat="identity",xlab=xlab,ylab=ylab) +  coord_flip() +

  if (show.ci) {
    es$abs.ci.low = es$ci.low * es$effect.sign
    es$abs.ci.high = es$ci.high * es$effect.sign
  }

  #p = qplot(data=es, y=abs.effect, x=name, fill=sign, geom="bar", stat="identity",xlab=xlab,ylab=ylab,...) + scale_fill_manual(values=colors)
  p = qplot(data=es, y=abs.effect, x=name, fill=sign, geom="bar", stat="identity",xlab=xlab,ylab=ylab, alpha=I(alpha),...) + scale_fill_manual(values=colors)

  if (horizontal)
    p = p+coord_flip()  #+ theme_wsj()
  
  if (show.ci) {
    p = p +geom_errorbar(aes(ymin=abs.ci.low, ymax=abs.ci.high), position="dodge", width=0.25, colour=gray(0.3, alpha=0.6))
  }
  
  if (!is.null(num.ticks)) {
    number_ticks <- function(n) {function(limits) pretty(limits, n)}
    p = p + scale_y_continuous(breaks=number_ticks(num.ticks))
  }
  
  if (add.numbers) {
    if (numbers.align=="left_of_bar_end") {
      p = p + geom_text(aes(x=name, y=abs.effect,
          ymax=max(abs.effect)*(1+right.margin), ymin=-(left.margin*max(abs.effect)),
          label=round.effect, hjust=1, vjust=numbers.vjust), 
          position = position_dodge(width=1))     
    } else if (numbers.align=="right_of_bar_end") {
       p = p + geom_text(aes(x=name, y=abs.effect,
          ymax=max(abs.effect)*(1+right.margin), ymin=-(left.margin*max(abs.effect)),
          label=round.effect, hjust=0,vjust=numbers.vjust))          
    } else {
      .POS = 0.5
      if (numbers.align == "left") {
        .POS = 0
      } else if (numbers.align=="center") {
        .POS = 0.5
      } else if (numbers.align=="right") {
        .POS = 1
      } else if (is.numeric(numbers.align)) {
        .POS = numbers.align
      }
      p = p + geom_text(aes(x = name, y = .POS*max(abs.effect),
                            ymax=max(abs.effect)*(1+right.margin),
                            ymin=-(left.margin*max(abs.effect)),
                            label=round.effect, hjust=0,vjust=numbers.vjust))
    }
  }
  #numbers.align = "left_of_bar_end","right_of_bar_end","center","left","right"  

  p
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

