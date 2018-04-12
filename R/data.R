#' Research Block Seabed Area
#' 
#' A dataset that contains fishable seabed area within Research Blocks assessed at WG-FSA-17 (see Fig 1 in WG-FSA-17/42)
#' 
#' @docType data
#' 
#' @format A dataframe with 27 rows and 2 columns
#' \describe{
#' \item{RB}{CCAMLR Research Block codes from Research Blocks on the CCAMLR online GIS as of 2018-04-10}
#' \item{Seabed_area}{Planimetric seabed area within the 600-1800 m estimated from a processed version of the GEBCO 2014 data - see inst/calculate_and_store_seabed_area_values.R}
#' }
"RB_seabed_area"

#' Reference Area Seabed Area
#' 
#' A dataset that contains fishable seabed area within Reference Areas that were used in used in estimating local biomass at WG-FSA-17 (see Fig 1 in WG-FSA-17/42)
#' 
#' @docType data
#' 
#' @format A dataframe with 6 rows and 2 columns
#' \describe{
#' \item{RefArea}{CCAMLR Reference area codes noting that RSR_open and HIMI are the only Reference areas currently used}
#' \item{Seabed_area}{Planimetric seabed area within the 600-1800 m estimated from a processed version of the GEBCO 2014 data - see inst/calculate_and_store_seabed_area_values.R}
#' }
"Ref_area_seabed_area"


#' Simulated catch data from hypothetical Research blocks for vignette examples
#' 
#' A dataset that aims to simulate the catch data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 400 rows and 8 columns
#' \describe{
#' \item{ID}{identifies a unique fishing event - i.e. both longline set and haul information}
#' \item{Season}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) that the longline was set and hauled}
#' \item{CRUISE_ID}{An ID used by Observers that is recorded with tagged fish releases}
#' \item{SET_ID}{An ID used by Observers that is recorded for each set-haul}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{CAUGHT_KG_TOTAL}{A simulated normally distributed catch that represents retained catch per longline haul}
#' \item{LINE_LENGTH}{A simulated normally distributed longline length}
#' \item{RESEARCH_BLOCK_CODE}{A simulated hypothetical Research block code that the data fall within}
#' }
"catch_data_sim_RB"


#' Simulated tagged fish release data from hypothetical Research blocks for vignette examples
#' 
#' A dataset that aims to simulate the tagged fish release data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 300 rows and 7 columns
#' \describe{
#' \item{SEASON}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) the tagged fish was released}
#' \item{CRUISE_ID}{An ID used by Observers that is recorded with tagged fish releases}
#' \item{SET_ID}{An ID used by Observers that is recorded for each released fish for each set-haul}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{RESEARCH_BLOCK_CODE}{A simulated hypothetical Research block code that the data fall within}
#' \item{ASD_CODE}{A simulated hypothetical CCAMLR Management area that represents an Area, Subarea or Division code that the data fall within}
#' \item{LENGTH_CM}{A simulated tagged and released toothfish length}
#' }
"release_data_sim_RB"


#' Simulated tagged fish recapture data from hypothetical Research blocks for vignette examples
#' 
#' A dataset that aims to simulate the tagged fish recapture data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 12 rows and 7 columns
#' \describe{
#' \item{SEASON_RELEASE}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) the tagged fish was released}
#' \item{SEASON_RECAPTURE}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) the tagged fish was recaptured}
#' \item{CRUISE_ID_RECAPTURE}{An ID used by Observers that is recorded with tagged fish recaptures}
#' \item{SET_ID_RECAPTURE}{An ID used by Observers that is recorded with recaptured fish for each set-haul}
#' \item{SPECIES_CODE_RECAPTURE}{The three letter code used by CCAMLR to record the species of a tagged fish recapture. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{RESEARCH_BLOCK_CODE_RECAPTURE}{A simulated hypothetical Research block code that the tagged fish recapture data fall within}
#' \item{RESEARCH_BLOCK_CODE_RELEASE}{A simulated hypothetical Research block code that the tagged fish release data fall within}
#' }
"recapture_data_sim_RB"

#' Simulated tagged fish length and weight data from hypothetical Research blocks for vignette examples
#' 
#' A dataset that aims to simulate the length and weight data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 400 rows and 4 columns
#' \describe{
#' \item{ASD_CODE}{A simulated hypothetical CCAMLR Management area that represents an Area, Subarea or Division code that the data fall within}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{LENGTH_CM}{A simulated retained catch toothfish length}
#' \item{WEIGHT_KG}{A simulated retained catch weight}
#' }
"length_weight_data_sim_RB"


#' Simulated catch data from Reference Areas for vignette examples
#' 
#'A dataset that aims to simulate the catch data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#'  
#' @format A dataframe with 800 rows and 8 columns
#' \describe{
#' \item{ID}{identifies a unique fishing event - i.e. both longline set and haul information}
#' \item{Season}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) that the longline was set and hauled}
#' \item{CRUISE_ID}{An ID used by Observers that is recorded with tagged fish releases}
#' \item{SET_ID}{An ID used by Observers that is recorded for each set-haul}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{CAUGHT_KG_TOTAL}{A simulated normally distributed catch that represents retained catch per longline haul}
#' \item{LINE_LENGTH}{A simulated normally distributed longline length}
#' \item{REF_AREA_CODE}{A simulated Reference Area that the data fall within that represents the current Reference Areas for Antarctic and Patagonian toothfish species}
#' }
"catch_data_sim_RefArea"


#' Simulated tagged fish release data from Reference Areas for vignette examples
#' 
#' A dataset that aims to simulate the tagged fish release data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 200 rows and 7 columns
#' \describe{
#' \item{SEASON}{The simulated CCAMLR fishing season (i.e. December from the previous calendar year through to the end of November from the current calendar year) the tagged fish was released}
#' \item{CRUISE_ID}{An ID used by Observers that is recorded with tagged fish releases}
#' \item{SET_ID}{An ID used by Observers that is recorded for each released fish for each set-haul}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{REF_AREA_CODE}{A simulated Reference Area that the data fall within that represents the current Reference Areas for Antarctic and Patagonian toothfish species}
#' \item{ASD_CODE}{A simulated hypothetical CCAMLR Management area that represents an Area, Subarea or Division code that the data fall within}
#' \item{LENGTH_CM}{A simulated tagged and released toothfish length}
#' }
"release_data_sim_RefArea"

#' Simulated tagged fish length and weight data from hypothetical Reference Areas for vignette examples
#' 
#' A dataset that aims to simulate the length and weight data necessary for toothfish local biomass estimates that is stored in CCAMLR's cdb database 
#' 
#' @docType data
#' 
#' @format A dataframe with 800 rows and  4 columns
#' \describe{
#' \item{ASD_CODE}{A simulated hypothetical CCAMLR Management area that represents an Area, Subarea or Division code that the data fall within}
#' \item{SPECIES_CODE}{The three letter code used by CCAMLR to record a species. In the simulated data example this represent either Antarctic toothfish = "TOA" or Patagonian toothfish = "TOP"}
#' \item{LENGTH_CM}{A simulated retained catch toothfish length}
#' \item{WEIGHT_KG}{A simulated retained catch weight}
#' }
"length_weight_data_sim_RefArea"
