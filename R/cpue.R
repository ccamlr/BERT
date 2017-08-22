## The subgroup agreed that the bootstrap would permit the following
##
## Permit depth stratification of CPUE
## Permit the specification in the number of years of CPUE to include
## Permit zero CPUE hauls
## Allow for the uncertainty in the estimated biomass in the reference area (assume lognormal with cv)
## Discuss vulnerable vs spawning biomass
##* stratification is just a loop


#' CPUE Seabed area analogy
#'
#' Calculate biomass using the CPUE seabed area analogy
#' @param fish_CPUE CPUE data from the research area
#' @param fish_area habitat area (km2) in research area
#' @param ref_CPUE CPUE data from reference area
#' @param ref_area habitat area in reference area
#' @param ref_bio biomass estimate in reference area
#' @export
cpue_bio <- function(fish_CPUE, fish_area, ref_CPUE ,ref_area, ref_bio){
  ## calculate the biomass
  bio <- (fish_CPUE * fish_area * ref_bio)/(ref_CPUE * ref_area)
  ## check replace infinity with NA
  if(is.infinite(bio)) bio <- NA
  ## return the biomass
  bio
}

#' CPUE Seabed area analogy
#'
#' Calculate biomass using the CPUE seabed area analogy
#' @param fish_CPUE_data vector of CPUE data from the research area
#' @param fish_area habitat area (km2) in research area
#' @param ref_CPUE_data vector of CPUE data from reference area
#' @param ref_area habitat area (km2) in reference area
#' @param ref_bio biomass estimate in reference area (I assume the units are in kg)
#' @param ref_bio_cv CV of biomass estimate in reference area
#' @param method method to calculate CPUE data either "mean" or "median" (default)
#' @export
CPUE_seabed <- function(fish_CPUE_data, fish_area, ref_CPUE_data,
                            ref_area, ref_bio, ref_bio_cv=0, method="median"){
  ## check for missing CPUE values
  if(any(is.na(fish_CPUE_data))) warning(paste0(length(fish_CPUE_data[which(is.na(fish_CPUE_data))]),
                                                " NA values in the CPUE data that have been removed"))
  if(any(is.na(ref_CPUE_data))) warning(paste0(length(ref_CPUE_data[which(is.na(ref_CPUE_data))]),
                                               " NA values in the CPUE data that have been removed"))
  ## remove NA CPUEs
  fish_CPUE_data <- fish_CPUE_data[!ia.na(fish_CPUE_data)]
  ref_CPUE_data <- ref_CPUE_data[!ia.na(ref_CPUE_data)]
  ## calculate the CPUEs
  ##** apply doens't work with vectors
  if(method=="median"){
    est_fish_CPUE <- median(fish_CPUE_data)
    est_ref_CPUE <- median(ref_CPUE_data)
  }else if(method=="mean"){
    est_fish_CPUE <- mean(fish_CPUE_data)
    est_ref_CPUE <- mean(ref_CPUE_data)
  }
  ## calculate the biomass
  bio <- cpue_bio(fish_CPUE = est_fish_CPUE, fish_area = fish_area,
                  ref_CPUE = est_ref_CPUE, ref_area = ref_area,
                  ref_bio = ref_bio)
  ## construct a list of the output
  res <- list(data = list(fish_CPUE = fish_CPUE_data,
                          ref_CPUE = ref_CPUE_data,
                          fish_area = fish_area,
                          ref_area = ref_area,
                          ref_bio = ref_bio,
                          ref_bio_cv = ref_bio_cv,
                          FUN=FUN),
              est = bio)
  ## add an S3 class
  class(res) <- "cpue_area"
  ## return the object
  res
}


#' CPUE Seabed area analogy (old implementation)
#'
#' Calculate biomass using the CPUE seabed area analogy
#' @param fish_CPUE_data vector of CPUE data from the research area
#' @param fish_area habitat area (km2) in research area
#' @param ref_CPUE_data vector of CPUE data from reference area
#' @param ref_area habitat area (km2) in reference area
#' @param ref_bio biomass estimate in reference area (I assume the units are in kg)
#' @param ref_bio_cv CV of biomass estimate in reference area
#' @export
CPUE_seabed_old <- function(fish_CPUE_data, fish_area, ref_CPUE_data,
                             ref_area, ref_bio, ref_bio_cv=0){
  ## check for missing CPUE values
  if(any(is.na(fish_CPUE_data))) warning(paste0(length(fish_CPUE_data[which(is.na(fish_CPUE_data))]),
                                           " NA values in the CPUE data that have been removed"))
  if(any(is.na(ref_CPUE_data))) warning(paste0(length(ref_CPUE_data[which(is.na(ref_CPUE_data))]),
                                           " NA values in the CPUE data that have been removed"))
  ## calculate the median CPUEs
  median_fish_CPUE <- median(fish_CPUE_data, na.rm=TRUE)
  median_ref_CPUE <- median(ref_CPUE_data, na.rm=TRUE)
  ## calculate the biomass
  bio <- cpue_bio(fish_CPUE = median_fish_CPUE, fish_area = fish_area,
                  ref_CPUE = median_ref_CPUE, ref_area = ref_area,
                  ref_bio = ref_bio)
  ## construct a list of the output
  res <- list(data = list(fish_CPUE = fish_CPUE_data,
                          ref_CPUE = ref_CPUE_data,
                          fish_area = fish_area,
                          ref_area = ref_area,
                          ref_bio = ref_bio,
                          ref_bio_cv = ref_bio_cv),
              est = bio)
  ## add an S3 class
  class(res) <- "cpue_area"
  ## return the object
  res
}

#' Bootstrap CPUE by seabed area
#'
#' Bootstrap CPUE by seabed area
#' @param x object of class cpue_area
#' @param nboot number of bootstrap samples (default=10000)
#' @param ... additional arguments
#' @export
bootstrap.cpue_area <- function(x, nboot = 1e4, ...){
  ## check the there are sufficient rows in the data
  if(length(x$data[["fish_CPUE"]]) <= 1) stop("there must be more than one CPUE record in the
                                          research area to undertake bootstrap")
  if(length(x$data[["fish_CPUE"]]) < 10) warning("there are less than 10 CPUE records
                                            in the research area used for the bootstrap")
  if(length(x$data[["ref_CPUE"]]) <= 1) stop("there must be more than one CPUE record in the
                                          reference area to undertake bootstrap")
  if(length(x$data[["ref_CPUE"]]) < 100) warning("there are less than 100 CPUE records
                                            in the reference area used for the bootstrap")
  ## create a vector to store the results
  boot_res <- rep(NA, nboot)
  ## loop over the bootstrap replicates
  for(i in 1:nboot){
    ##* tidy this up
    boot_fish_CPUE <- median(sample(x$data[["fish_CPUE"]], 
                                    length(x$data[["fish_CPUE"]]), 
                                    replace=TRUE), na.rm=TRUE)
    boot_ref_CPUE <- median(sample(x$data[["ref_CPUE"]],
                                   length(x$data[["ref_CPUE"]]),
                                   replace=TRUE), na.rm=TRUE)
    boot_ref_bio <- rlnorm(1, meanlog=log(x$data[["ref_bio"]]),
                           sdlog=sqrt(log((x$data[["ref_bio_cv"]]^2)+1)))
    ## Calculate the biomass in the fished area
    boot_res[i] <- cpue_bio(fish_CPUE = boot_fish_CPUE, 
                            fish_area = x$data[["fish_area"]],
                            ref_CPUE = boot_ref_CPUE, 
                            ref_area = x$data[["ref_area"]],
                            ref_bio = boot_ref_bio)
  }
  ## create an output list
  obj <- list("cpue_area_obj" = x,
              "Boot_estimates" = boot_res)
  ## add a class
  class(obj) <- "cpuesamples"
  ## return the results
  obj
}

#' S3 method for bootstrapped confidence intervals
#'
#' Calculate bootstrapped confidence intervals for object of
#' class cpuesamples
#' @param object object of class bsamples
#' @param quantiles bootstrap quantiles (default 0.025, 0.5, 0.975)
#' @param ... additional parameters
#' @export
summary.cpuesamples <- function(object, quantiles=c(0.025, 0.5, 0.975), ...){
  ## define the quantiles
  quants <- quantile(object$Boot_estimates, 
                     probs=quantiles, na.rm=TRUE)
  se <- sd(object$Boot_estimates, na.rm=TRUE)
  cv <- se / object$cpue_area_obj$est
  est <- object$cpue_area_obj$est
  names(se) <- "boot_SE"
  names(cv) <- "boot_CV"
  names(est) <- "Est"
  ## construct the output
  out <- c(est, se, cv, quants)
  # return the bootstrapped estimates
  out
}
