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
