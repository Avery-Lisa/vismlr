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
