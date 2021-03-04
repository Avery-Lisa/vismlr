#' create a subsample that preserves group proportions
#' @param data a data.frame
#' @param  group a character vector identifying the group column
#' @param p value between 0 and 1 specifying the proportion of rows to return
#' @noRd
getSubSample <-function(data,group,p=.5){
  if (! group %in% names(data)) stop(paste(group,'is not found in the data.'))
  if (0>p | p>1) stop('p must be between 0 and 1')
  subsample = NULL
  for (g in unique(data[[group]])){
    subsample = rbind(subsample,
                      data[sample((1:nrow(data))[data[[group]] == g], p*sum(data[[group]]==g),replace=F), ])
  }
  return(subsample)
}

#' standardises all the predictor variables from a glm fit
#' @param glm_fit an object return from glm
#' @param  id_var a character identifying a column in the fitting dataset with identifiers
#' @param ordered logical, indicating if the ids should be ordered according to the fitted value
#' @noRd
getstddf <- function(glm_fit,id_var=NULL,ordered=T){
  data=glm_fit$model
  responseVar = names(data)[1]
  # Step 1: Standardise all the values to lie between 0 and 1
  df <- NULL
  i=2
  for (i in 2:ncol(data)){
    if (class(data[[i]])[1] %in% c('factor','character')) {
      data[[i]]= as.numeric(factor(data[[i]]))
    }
    max_x=max(data[,i],na.rm=T)
    min_x=min(data[,i],na.rm=T)
    range_x=max_x-min_x
    df <- cbind(df,(data[,i]  -min_x)/range_x)
  }

  colnames(df) = names(data)[2:ncol(data)]

  if (is.null(id_var)) {
    id_vals =   1:nrow(df)
  }  else {
    id_vals = glm_fit$data[[id_var]][as.numeric(names(glm_fit$y))]
  }
  dat <- data.frame(ID=factor(id_vals),
                    Class=factor(data[,1]),
                    df)

  if (ordered)  dat$ID = forcats::fct_reorder(dat$ID,glm_fit$fitted.values,min)
  names(dat)[3:ncol(dat)] <- names(data)[2:ncol(data)]
  return(dat)
}

#' creates a long form data set with a row for each person/predictor
#' @param glm_fit an object return from glm
#' @param  id_var a character identifying a column in the fitting dataset with identifiers
#' @param n integer specifying how many participants to include in the dataset, defaults to all
#' @param random logical if n is specified random=T randomly selects n, otherwise first n selected (maintain group proportions)
#' @param maxn the maximum number of participants to return in the dataset
#' @noRd
getlongdf <- function(glm_fit,id_var=NULL,n=NULL,random=T,maxn=50){

  dat=getstddf(glm_fit,id_var,ordered = T)

  # refit model on std scores to determine item ordering
  std_model <- cbind(glm_fit$model[,1],dat[,3:ncol(dat)])
  names(std_model) <- names(glm_fit$model)
  names(std_model)[1] <-'y'
  std_fit <- stats::glm(y~.,data=std_model,family='binomial')
  names(std_model) <- names(glm_fit$model)
  xvar_order = data.frame(variable=gsub('`','',names(std_fit$coefficients)), est = unname(std_fit$coefficients))
  #  std_fit <- stats::update(glm_fit,data=std_model)

  if (is.null(n)) {
    if (nrow(dat)>maxn) n=maxn else n=nrow(dat)
  } else {
    if (n>maxn) n=maxn
  }

  if (n<nrow(dat)) {
    if (random){
      dat = getSubSample(dat,group="Class",p=n/nrow(dat))
    } else {
      nbygrp = floor(n*prop.table(table(dat$Class)))
      new_dat = NULL
      for (i in names(nbygrp)){
        new_dat = rbind(new_dat, dat[which(dat$Class==i)[1:nbygrp[i]],])
      }
      dat=new_dat
    }
  }
  dat_long =tidyr::pivot_longer(data = dat, cols=!c('ID','Class'),names_to='variable')
  dat_long <- merge(dat_long,xvar_order,all.x = TRUE)
  dat_long$variable = forcats::fct_reorder(factor(dat_long$variable),
                                           dat_long$est,
                                           min)
  return(dat_long[,1:4])
}

niceNum <- function(x,digits=2){

  rndx = sapply(x, function(x) {format(round(as.numeric(x),digits),nsmall=digits)})
  return(gsub(" ","",rndx))
}

format_glm = function(glm_fit,conf.level = 0.95,digits=c(2,3),orderByRisk=TRUE){

  if (! class(glm_fit)[1] %in% c('glm','polr')) stop('Only objects of class glm and polr are accepted.')

  #extracting ORs and p values
  tab = broom::tidy(glm_fit,conf.int = TRUE, exponentiate = TRUE,conf.level=conf.level)
  tab$estimate.label = paste0(niceNum(tab$estimate), ' (',niceNum(tab$conf.low),', ',niceNum(tab$conf.high),')')

  if (class(glm_fit)[1]=='glm'){
    tab = tab[-which(tab$term=='(Intercept)'),]
  }  else {
    tab <- tab[tab$coef.type=='coefficient',]

    pvals = broom::tidy(glm_fit)
    pvals$p.value =  pnorm(abs(pvals$estimate/pvals$std.error),lower.tail = FALSE) * 2
    pvals <- pvals[pvals$coef.type=='coefficient',]
    pvals <- pvals[,c('term','p.value')]
    tab <- merge(tab,pvals)
  }


  tab$p.label = ifelse(tab$p.value<0.001, '<0.001', niceNum(tab$p.value,digits[2]))
  names(tab)[1] = 'variable'

  tab = tab[,c('variable', 'estimate', 'p.label', 'p.value', 'conf.low', 'conf.high')]


  if (orderByRisk){
    tab$var.order = rank(tab$estimate)
  } else{
    tab$var.order = 1:nrow(tab)
  }

  # Extract the reference levels if needed
  if (length(glm_fit$xlevels)!=0){
    ref_levels <- NULL
    for (i in seq_along(glm_fit$xlevels)){
      ref_levels <- rbind(ref_levels,
                          data.frame(var.name=rep(names(glm_fit$xlevels)[i],length(glm_fit$xlevels[[i]])+1),
                                     level.name = c(names(glm_fit$xlevels)[i],glm_fit$xlevels[[i]]),
                                     level.order=1:(length(glm_fit$xlevels[[i]])+1),
                                     variable=paste0(names(glm_fit$xlevels)[i],c('',glm_fit$xlevels[[i]]))))
    }


    tab = merge(ref_levels, tab, by='variable',all = T)

    tab$estimate.label = ifelse(is.na(tab$estimate), '1.0 (Reference)',
                                paste0(niceNum(tab$estimate), ' (',niceNum(tab$conf.low),', ',niceNum(tab$conf.high),')'))

    varOrders <- tapply(X = tab$var.order,
                        INDEX=tab$var.name,
                        FUN = function(x) min(x,na.rm=T))
    varOrderLookup <- data.frame(var.name=names(varOrders),var.order=varOrders)


    varOrderLookup <- stats::na.omit(tab[,c("var.name","var.order")])

    for (i in 1:nrow(varOrderLookup)){
      tab$var.order[tab$var.name==varOrderLookup$var.name[i]] <- varOrderLookup$var.order[i]
    }

    tab$estimate.label = ifelse(tab$level.name %in% names(glm_fit$xlevels),NA_character_,tab$estimate.label)
    tab[order(tab$var.order,tab$level.order,decreasing=c(F,T)),]
  } else {
    tab$estimate.label = paste0(niceNum(tab$estimate), ' (',niceNum(tab$conf.low),', ',niceNum(tab$conf.high),')')
    tab$level.order=1
    tab$var.name=tab$variable
    tab$level.name=tab$variable
    tab[order(tab$var.order),]
  }

}
