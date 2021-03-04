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
