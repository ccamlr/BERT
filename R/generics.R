## ************************************************ ##
## New S3 Methods

#' Bootstrap method for single release tag-return studies
#'
#' Bootstrap method for single release tag-return studies
#' 
#' Estimate confidence intervals using a non-parametric bootstrap. This method
#' incoporates uncertainty in tag-induced mortality, natural mortality and tag 
#' shedding as a series of Bernoulli trials. @seealso \code{\link{single_release}}
#' for more details.
#' @param x an object of class srelease
#' @param nboot number of bootstrap samples
#' @param boot_zeroes should bootstrap replicates with zero tag-recaptures be
#' used to estimate population size (default=TRUE)
#' @aliases tag_bootstrap.srelease tag_bootstrap.mrelease
#' @export
tag_bootstrap <- function(x, nboot, boot_zeroes=TRUE)
  UseMethod("tag_bootstrap")


#' Estimate confidence intervals using a non-parametric bootstrap.
#' @param x an object of of the appropriate class
#' @param nboot number of bootstrap samples
#' @aliases cpue_bootstrap.cpue_area
#' @export
cpue_bootstrap <- function(x, nboot)
  UseMethod("cpue_bootstrap")

#' Test for integer zero
#'
#' Test for integer zero
#' @param x an object to text for property
#' @export
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

