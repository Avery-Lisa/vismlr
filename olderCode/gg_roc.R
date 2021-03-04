#'This function will accept a logistic glm and plot the ROC
#'TODO: Not robust - this needs to be updated to allow for missing data.
#'
#' @param glm_fit an object output from the glm function, must be from a logistic regression
#' @param showAUC logical, should the AUC be plotted on the graphe
#' @param showCut logical, should the cut point be shown on the graph
#' @param plotOnly logical, should both the plot and the AUC be returned
#' @param title character string with graph title, defaults to the calling function
#' @import ggplot2
#' @importFrom ROCR prediction performance
#' @keywords plot
#' @export
#'
gg_roc <- function(glm_fit,showAUC=T,showCut=F,plotOnly=F,title){

  class_p <- predict(glm_fit,type='response')
  data = glm_fit$model
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
  if (missing(title)) title = as.character(glm_fit$call)[2]
  if (!is.null(title)){
    p = p + ggtitle(title) + theme(plot.title = element_text(size=11))
  }
  if (plotOnly){
    return(p)
  } else {
    return(list(plot=p,auc=auc))
  }
}
