#' Survival data from the Titanic
#'
#' A dataset containing survival status and predictor information for
#' 1309 passengers from the maiden voyage of the Titanic.
#'
#' @format A data frame with 1309 rows and 14 variables:
#' \describe{
#'   \item{pclass}{integer, passenger class (first, second third)}
#'   \item{survived}{1 if passenger survived, 0 otherwise}
#'   \item{name}{passenger's full name}
#'   \item{sex}{passenger's sex}
#'   \item{age}{passenger's age}
#'   \item{sibsp}{number of siblings/spouse travelling with passenger}
#'   \item{parch}{number of parents/children travelling with passenger}
#'   \item{ticket}{ticket number}
#'   \item{fare}{amount paid for ticket}
#'   \item{cabin}{passenger's location on the ship}
#'   \item{embarked}{port of entry to the ship}
#' }
#' @source \url{https://www.kaggle.com/vinicius150987/titanic3}
"titanic"
