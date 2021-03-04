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
