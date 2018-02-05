## test check_cpue_bio inputs

check_cpue_bio <- function(fish_CPUE, fish_area, ref_bio, ref_CPUE,ref_area){
  ## define the check variable
  check <- TRUE
  ## check if any inputs are < 0
  if(any(fish_CPUE<=0, fish_area<=0, ref_bio<=0,ref_CPUE<=0,ref_area<=0)){
    check <- FALSE
    warning("CPUE, planimetric area and reference biomass must not be zero or negative")
  }
  ## return check
  check
}

