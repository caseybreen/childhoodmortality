#' Model DHS dataset
#'
#' A model open-use dataset created by the DHS program for users to become familiar with datasets without having to register for access.
#'
#'
#' @format A data frame with 8348 rows and 9 variables:
#' \describe{
#'   \item{KIDDOBCMC}{KIDDOBCMC (B3) reports the century month code for the date of birth of the child.}
#'   \item{YEAR}{YEAR reports the year when the survey was fielded}
#'   \item{PSU}{PSU (V021) is the variable indicating the primary sampling unit or PSU}
#'   \item{PERWEIGHT}{PERWEIGHT (V005) is a weighting factor to produce representative estimates}
#'   \item{KIDAGEDIEDIMP}{KIDAGEDIEDIMP (B7) reports the age of the child at death in months (including imputed ages)}
#'   \item{INTDATECMC}{INTDATECMC (V008) reports century month code for the date on which the interview took place)}
#'   \item{WEALTHQ}{WEALTHQ (V190) refers to the relative wealth of the household where the woman lives, divided into
#'    quintiles from the poorest (code 1) to the richest (code 5))}
#'   ...
#' }
#' @source \url{https://www.dhsprogram.com/data/Model-Datasets.cfm}
"model_ipums_dhs_dataset"

