#' Research Block Seabed Area
#'  
#'  A dataset that contains fishable seabed area estimated from a 
#'   processed version of the GEBCO 2014 data within Research Blocks that were assessed at WG-FSA-17 (see Fig 1 in WG-FSA-17/42)
#' @format A dataframe with 27 rows and 2 columns
#' \describe{
#' \item{RB}{CCAMLR Research Block codes from WG-FSA-17}
#' \item{Seabed_area}{Planimetric seabed area within the 600-1800 m fishable depth range}
#' }
"RB_seabed_area"

#' Reference Area Seabed Area
#'  
#'  A dataset that contains fishable seabed area estimated from a processed version of the GEBCO 2014 data
#'   within Reference Areas that were used in used in estimating local biomass at WG-FSA-17 (see Fig 1 in WG-FSA-17/42)
#' @format A dataframe with 6 rows and 2 columns
#' \describe{
#' \item{RefArea}{CCAMLR Reference area codes noting that RSR_open and HIMI are the only Reference areas currently used}
#' \item{Seabed_area}{Planimetric seabed area within the 600-1800 m fishable depth range}
#' }
"Ref_area_seabed_area"
