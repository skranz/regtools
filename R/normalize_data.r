
examples.change.data.units = function() {
  setwd("D:/libraries/RTutor2/examples")
  var.dt = read.var.txt("bank runs variables.txt")
  term = read.dta("term_deposit_data_file1.dta") 
  term = translate.var.names(term, var.dt=var.dt)
  
  dat = term
  dat$account_years = dat$account_days / 365
  datn = change.data.units(dat,"sd")
  attributes(datn[[1]])
  regn = lm(runner_d~minority+balance+account_years+has_loan, data=datn)
  regn
  datn = change.data.units(dat,"25-75")
  regn = lm(runner_d~minority+balance+account_years+has_loan+above_insured, data=datn)
  regn
  coefplot(regn, data=datn)
  
  coef = coef(regn)
  
  ord = order(abs(coef()))
  sort(coef(regn))
  
  reg = lm(runner_d~minority+above_insured+account_days+has_loan, data=dat)
  reg

  
  reg = pe42
  reg
  library(effects)
  
  plot(effect(c("has_loan", "minority"), reg))
  names(dat)
  names(dat)
}

#' Change units of columns in a data frame, e.g. measure relative to one standard deviation
#' 
#' useful to make coefficients in regressions better comparable

change.data.units = function(dat,unit="sd",cols = setdiff(colnames(dat),exclude), exclude=NULL, dummy01=TRUE, lowest=0) {
  
  units = lapply(colnames(dat), function(col) {
    list(unit = "original",size=1, descr="") 
  })
  dummy.unit = list(unit="dummy",size=1,descr="unit: dummy")    
    
    
  unit.val = rep
  names(units)=cols
  for (col in cols) {
    val = dat[[col]]
    if (!is.numeric(val)) {
      attr(dat[[col]],"unit") = dummy.unit
      next
    } 
    if (dummy01) {
      if (setequal(unique(val),c(0,1))) {
        attr(dat[[col]],"unit") = dummy.unit
        next
      }
    }
    nval = change.val.unit(val, unit=unit, lowest=lowest, var.name=col)
    dat[[col]] = nval
  }
  attr(dat,"units") = units
  dat
}

change.val.unit = function(val, 
    unit=c("sd","1sd","2sd","4sd","6sd","10-90","20-80","25-75","min-max","nochange"),
    lowest=0, var.name=""
) {
  restore.point("change.val.unit")
  #unit = "10-90"; val = dat$account_days
  if (unit=="original") {
    attr(val,"unit")=list(unit = "original",size=1, descr="") 
    return(val)
  }
  
  org.val = val
  val = na.omit(val)
  unit = unit[1]
  if (unit=="sd")
    unit="1sd"
  if (substring(unit,2)=="sd") {
    denom = sd(val) * as.numeric(substring(unit,1,1))
  } else if (unit=="min-max") {
    denom = max(val)-min(val)
  } else {
    start = as.numeric(substring(unit,1,2))
    end = as.numeric(substring(unit,4,5))
    denom = diff(quantile(val, c(start, end)/100))
    if (denom==0) {
      warning("Normalize ", var.name," with unit ", unit, " has a zero denominator!")
    }
  }
  ret = (org.val-lowest-min(val)) / denom
  attr(ret,"unit") = list(unit=unit, size=denom, descr=paste0("unit ", unit, ": ", signif(denom,3)))
  ret
}

#' Graphically compare sizes of regression coefficient
#' 
#' mainly useful if regression is run on data that has been normalized by set.data.units
#' 
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

  qplot(data=df, y=abs.coef, x=name, fill=sign, geom="bar", stat="identity") +  coord_flip() + theme_wsj()
}
