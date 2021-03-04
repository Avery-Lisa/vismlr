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
