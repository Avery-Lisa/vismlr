#'This function will accept a log or logistic regression fit from glm, and display the
#'OR or RR for each variable
#'
#' @param glm_fit an object output from the glm function, must be from a logistic regression
#' @param conf.level controls the width of the confidence interval
#' @param orderByRisk logical, should the plot be ordered by risk
#' @param colours can specify colours for risks less than, 1 and greater tham 1.0. Default is red, black, green
#' @param showEst logical, should the risks be displayed on the plot in text
#' @import ggplot2
#' @keywords plot
#' @export
#' @examples
#' titanic_fit = glm(survived~pclass+age+sex+fare,data=titanic,family = 'binomial')
#' riskplot(titanic_fit)
#'
riskplot = function(glm_fit,conf.level=0.95,orderByRisk=T,colours='default',showEst=T){

  if (class(glm_fit)[1]=='glm'){
    if(glm_fit$family$link=='log'){
      x_lab = 'Relative Risk'
    } else if (glm_fit$family$link=='logit'){
      x_lab='Odds Ratio'
    } else stop('glm_fit must be a logit or log link fit')
  } else {
    x_lab='Odds Ratio'
  }

  tab = format_glm(glm_fit,conf.level = conf.level,orderByRisk=orderByRisk)
  tab <- tab[!is.na(tab$estimate.label),]
  # yMax = nrow(tab) + length(unique(tab$var.order)) -1
  yvals=1
  for (i in 2:nrow(tab)) {
    yvals = c(yvals, ifelse(tab$var.order[i]==tab$var.order[i-1],
                            yvals[i-1]+.5,
                            yvals[i-1]+1))
  }
  tab$estimate.label = ifelse(is.na(tab$estimate.label),'',tab$estimate.label)
  tab$estimate.label = ifelse(tab$estimate.label == '1.0 (Reference)','(Reference)',tab$estimate.label)

  if (showEst){
    yLabels = data.frame(y.pos=yvals,
                         labels=ifelse(is.na(tab$level.name),
                                       paste(tab$variable,'\n',tab$estimate.label),
                                       paste(tab$level.name,ifelse(tab$estimate.label == '(Reference)','','\n'),tab$estimate.label)))
  } else {
    yLabels = data.frame(y.pos=yvals,
                         labels=ifelse(is.na(tab$level.name),
                                       tab$variable,
                                       ifelse(tab$estimate.label == '(Reference)',
                                              paste(tab$level.name,tab$estimate.label),
                                              tab$level.name)
                         ))
  }
  yLabels$labels <- gsub('_',' ',yLabels$labels)
  yLabels <- yLabels[!is.na(yLabels$y.pos),]
  # TODO: add indents using '  ' to category labels, omit RR estimates?, make hjust=0

  tab$x.val = ifelse(tab$estimate.label == '(Reference)',1,tab$estimate)
  tab$y.val = yLabels$y.pos

  tab$colour <- ifelse(tab$x.val<1,'a',ifelse(tab$x.val==1,'b','c'))

  if (colours=='default'){
    colours = c(a='red',b='black',c='darkgreen')
  }  else {
    names(colours) = c('a','b','c')
  }

  # ensure that colours are always red, black, green
  colours <- colours[sort(unique(tab$colour))]

  p = ggplot(tab, aes_(x=~x.val,y=~y.val,colour=~colour))+
    geom_point(na.rm=T,size=2) +
    geom_errorbarh(aes_(xmin = ~conf.low, xmax = ~conf.high),
                   height  = 0,
                   size   = 0.9,
                   na.rm=T) +
    geom_vline(xintercept = 1.0) +
    labs(y='',x=x_lab) +
    guides(colour='none')+
    scale_y_continuous(breaks = yLabels$y.pos,labels=yLabels$labels) +
    scale_colour_manual(values=colours)+
    theme_bw() +
    theme(axis.text.y = element_text(hjust=0))

  p
}

#'This function will accept a log or logistic regression fit from glm, and display the
#'details associated with predicting the class from the model
#'
#' @param glm_fit an object output from the glm function, must be from a logistic regression
#' @param responseLabels a character vector of the response labels ordered 0,1
#' @param digits number of digits to diplay in the output
#' @export
#' @examples
performancetable <- function(glm_fit,responseLabels=NULL,digits=2){
  if(glm_fit$family$link=='log'){
    x_lab = 'Relative Risk'
  } else if (glm_fit$family$link=='logit'){
    x_lab='Odds Ratio'
  } else stop('glm_fit must be a logit or log link fit')

  obs = glm_fit$model[,1]
  if (length(unique(obs))>2) stop('Function currenly only implemented for binary response.')

  pred = ifelse(predict(glm_fit,type='response')<0.5,0,1)

  a = sum(obs==1 & pred==1)
  b= sum(obs==0 & pred==1)
  c = sum(obs==1 & pred==0)
  d= sum(obs==0 & pred==0)

  rtn = c(
    Prevalence = (a+c)/(a+b+c+d),
    Sensitivity = a/(a+c),
    Specificity = d/(b+d),
    PPV = a/(a+b),
    NPV = d/(d+c),
    Accuracy = (a+d)/(a+b+c+d)
  )
  out = data.frame(Staistic = names(rtn),
                   value=round(rtn,digits))
  rownames(out) <- NULL
  out
}

#'This function will accept a logistic regression fit from glm, standardised all the
#'explanatory variables, and plot separate correlation heatmaps for each
#'classification category.
#'
#' @param glm_fit an object output from the glm function, must be from a logistic regression
#' @param responseLabels character vector with names to use for the response, in the order 0,1, optional
#' @param id_var specify a column name to identify plots by, defaulti s sequential labelling
#' @param highlight logical indicating whether the largest differences should be highlighted
#' @param addNum logical, should numbers be added to the explanatory variables to identify them? Useful for large models
#' @param nmLen optional integer specifying how many characters should be retained in the x-axis labels.
#' @param rhoD defaults to 0.2, the minimum difference in rank correlation required to highlight cells
#' @import ggplot2
#' @keywords plot
#' @export
#' @examples
#' titanic_fit = glm(survived~pclass+age+sex+fare,data=titanic,family = 'binomial')
#' plotheatcor(titanic_fit,responseLabels=c('perished','survived'))
#'
plotheatcor<-function(glm_fit,responseLabels=NULL,id_var=NULL,highlight=T,addNum=F,nmLen=4,rhoD=0.2){
  pd <- pd_diff <- NULL
  Var2 <- Var1 <- value <- group <-Agree <- NULL
  data=glm_fit$model
  responseVar = names(data)[1]

  if (is.null(id_var)) {
    id_vals =   1:nrow(data)
  }  else {
    id_vals = glm_fit$data[[id_var]][as.numeric(names(glm_fit$y))]
  }
  dat <- data.frame(ID=factor(id_vals),
                    Class=factor(data[,1]),
                    data[,2:ncol(data)])


  dataMat = dat[,-1]
  groupvar=names(dataMat)[1]
  if (!is.null(groupvar)){
    varind = which(colnames(dataMat)==groupvar)
    n_grp = length(unique(dataMat[[groupvar]]))
    pd <- NULL
    for (i in unique(dataMat[[groupvar]])){
      grpdat <- dataMat[dataMat[[varind]]==i,-varind]
      grpdat <- Filter(is.numeric,grpdat)
      corMat = stats::cor(grpdat,method = 'spearman')
      upperTri = upper.tri(corMat)
      dimnames(upperTri) = dimnames(corMat)
      keepInd = reshape2::melt(upperTri)[,3]

      pd=rbind(pd,cbind(group=i,reshape2::melt(corMat)[keepInd,]))
    }

  } else {
    corMat = stats::cor(grpdat,method = 'spearman')
    upperTri = upper.tri(corMat)
    dimnames(upperTri) = dimnames(corMat)
    keepInd = reshape2::melt(upperTri)[,3]

    pd = reshape2::melt(corMat)[keepInd,]
  }

  if (addNum){
    nameMap1 <- data.frame(Var1 = unique(pd$Var1),
                           newnm = factor(paste(1:length(unique(pd$Var1)),unique(pd$Var1)),
                                          levels=paste(1:length(unique(pd$Var1)),unique(pd$Var1)),
                                          ordered = T))
    nameMap2 <- data.frame(Var2 = unique(pd$Var2),
                           shrtnm = factor(substring(paste(1:length(unique(pd$Var2)),unique(pd$Var2)),1,nmLen),
                                           levels=substring(paste(1:length(unique(pd$Var2)),unique(pd$Var2)),1,nmLen),
                                           ordered=T))
    pd <- dplyr::full_join(pd,nameMap1)
    pd <- dplyr::full_join(pd,nameMap2)
    pd$Var1 = pd$newnm
    pd$Var2 = pd$shrtnm
  }

  if (!is.null(responseLabels)) {
    pd$group=factor(pd$group,labels=responseLabels)
  }
  p <- ggplot(data=pd,
              aes(x=Var2,y=Var1,fill=value)) +
    geom_raster() +
    labs(x='',y='',fill='') +
    colorspace::scale_fill_binned_diverging() +
    theme_bw()+
    theme(axis.text.x=element_text(angle =90, vjust = 0.5))
  if (!is.null(groupvar)) p = p+ facet_wrap(~group)

  if (!is.null(responseLabels)) {
    p = p + scale_color_discrete(labels=responseLabels)
  }
  if (highlight){
    pd_diff = tidyr::pivot_wider(data=pd,names_from=group,values_from=value)
    pd_diff = cbind(pd_diff,
                    D=unname(abs(pd_diff[,3]-pd_diff[,4])),
                    Agree=unname((pd_diff[,3]*pd_diff[,4])>0))

    pd_diff <- pd_diff[pd_diff$D>rhoD,]
    pd_lbl <- tidyr::pivot_longer(pd_diff,cols=3:4,names_to='group',values_to='value')
    p = p +
      geom_point(data=pd_lbl,aes(shape=Agree)) +
      guides(shape=FALSE)
  }

  p
}

#'This function will accept a logistic regression fit from glm, standardised all the
#'explanatory variables, order them by median value and output individual plots coloured by
#'classification category.
#'
#' @param glm_fit an object output from the glm function, must be from a logistic regression
#' @param id_var character specifying the colname containing the identifying data in the original dataset used to fit the model
#' @param responseLabels character vector with names to use for the response, in the order 0,1, optional
#' @param n number of participants to include, default is all
#' @param random logical, the number of participants will be randomly selected or if random=F the first n will be used
#' @param maxplots the maximum number of individual plots to display
#' @import ggplot2
#' @keywords plot
#' @export
#' @examples
#' titanic_fit = glm(survived~pclass+age+sex+fare,data=titanic,family = 'binomial')
#' plotpatterns(titanic_fit)
#'
plotpatterns<-function(glm_fit,id_var=NULL,responseLabels=NULL,n=NULL,random=T,maxplots=50){

  dat <- getlongdf(glm_fit,id_var,n,random,maxn=maxplots)

  p = ggplot(dat, aes(x=.data$value,y=.data$variable,colour=factor(.data$Class))) +
    geom_point(alpha=.5) +
    labs(x='',y='',col='') +
    facet_wrap(~ID) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_blank(),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
          strip.text = element_text(size=rel(0.75)),
          strip.background = element_rect(fill="lightgrey", colour="black",size=.75))
  if (!is.null(responseLabels)) {
    p = p + scale_color_discrete(labels=responseLabels)
  }
  p



}


#'This function will accept a logistic glm and plot the ROC
#'TODO: Not robust - this needs to be updated to allow for missing data.
#'
#' @param fitted_obj either an object output from the glm function, must be from a logistic regression, or a dataframe in the format (pred,reference)
#' @param showAUC logical, should the AUC be plotted on the graph
#' @param showCut logical, should the cut point be shown on the graph
#' @param plotOnly logical, should both the plot and the AUC be returned
#' @param title character string with graph title, defaults to the calling function
#' @import ggplot2
#' @importFrom ROCR prediction performance
#' @keywords plot
#' @export
#'
gg_roc <- function(fitted_obj,showAUC=T,showCut=F,plotOnly=F,title){
  if (class(fitted_obj)[1]=='glm'){
    class_p <- predict(fitted_obj,type='response')
    data = fitted_obj$model
  } else {
    class_p = fitted_obj[,1]
    data = fitted_obj[,2]
  }
  pred <- ROCR::prediction(class_p,data[,1])
  perf_obj <- ROCR::performance(pred,"tpr","fpr")
  df =  data.frame(x=perf_obj@x.values[[1]],
                   y=perf_obj@y.values[[1]],
                   Youden = perf_obj@y.values[[1]]-perf_obj@x.values[[1]],
                   cutoff = perf_obj@alpha.values[[1]])
  cutpoint = df$cutoff[which.max(df$Youden)]
  auc <- niceNum(ROCR::performance(pred,"auc")@y.values[[1]][1],2)
  p = ggplot(data =df,
             aes(x=x,y=y)) +
    geom_line() +
    xlab(perf_obj@x.name) +
    ylab(perf_obj@y.name)
  if (showAUC){
    p = p + annotate(geom='text',x=.8,y=.1,label=paste('AUC =',auc))
  }
  if (showCut){
    p = p+  geom_segment(
      aes(x = .5, y = .5, xend = df$x[which.max(df$Youden)], yend = df$y[which.max(df$Youden)]),
      arrow = arrow(length = unit(0.03, "npc")))
    p = p + annotate(geom='text',x=.5,y=.5,label=paste('cutpoint =',round(cutpoint,2)),hjust=0,vjust=1)
  }
  if (missing(title)) title = as.character(fitted_obj$call)[2]
  if (!is.null(title)){
    p = p + ggtitle(title) + theme(plot.title = element_text(size=11))
  }
  if (plotOnly){
    return(p)
  } else {
    return(list(plot=p,auc=auc))
  }
}
