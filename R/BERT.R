#' BERT: Biomass Estimation for Research on Toothfish
#'
#' The BERT package provides methods for calculating precautionary estimates
#' of biomass using CPUE and tag-based methods to prevent stock depletion while
#' progressing towards an assessment.
#'
#' @section Multiple release, single release tag-return models:
#' Implementation of Chapman estimators (Seber 1982)
#' along with bootstrapped confidence intervals
#'
#' @section CPUE seabed area:
#' Implementation of the CPUE by seabed area analogy method (Agnew et al, 2009)
#' with boostrapped confidence intervals
#'
#' @section Vignettes:
#' To learn more about BERT, start with the vignette: 
#'
#'
#'
#' @docType package
#' @name BERT
#' @importFrom grDevices dev.off png terrain.colors
#' @importFrom graphics par
#' @importFrom raster crop extent freq mask raster xres yres plot
#' @importFrom stats median rlnorm quantile rbinom sd
#' @importFrom utils download.file read.csv
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
NULL

## quiets concerns of R CMD check re: the .'s that appear in ddply
# if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
