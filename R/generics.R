#' S3 generic bootstrap method
#'
#' Estimate confidence intervals using a non-parametric bootstrap.
#' @param x an object of of the appropriate class
#' @param nboot number of bootstrap samples
#' @param boot_zeroes specifing whether to include bootstrap samples with zero recaptures
#' @export
bootstrap <- function(x, nboot, boot_zeroes)
  UseMethod("bootstrap")

#' Test for integer zero
#'
#' Test for integer zero
#' @param x an object to text for property
#' @export
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

