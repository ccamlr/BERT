#' BERT: Biomass Estimation for Research on Toothfish
#'
#' The BERT package provides methods for calculating precautionary estimates
#' of biomass using CPUE and tag-based methods to prevent stock depletion while
#' progressing towards an assessment.
#'
#' @section Multiple release, single recapture tag-return models:
#' Implementation of Chapman estimators (Seber 1982)
#' along with bootstrapped confidence intervals
#'
#' @section CPUE seabed area:
#' Add this section
#'
#' @section Vignettes:
#' To learn more about BERT, start with the vignettes:
#'
#'
#'
#' @docType package
#' @name BERT
#' @importFrom grDevices dev.off png terrain.colors
#' @importFrom graphics par
#' @importFrom raster crop extent freq mask raster xres yres plot
#' @importFrom stats median rlnorm
#' @importFrom utils download.file read.csv
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
NULL

## quiets concerns of R CMD check re: the .'s that appear in ddply
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
