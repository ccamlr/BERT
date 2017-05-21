## extract and process data from CCAMLR database extracts

#' Converts a field to a date in R
#'
#' Takes a dataframe and converts one of the columns to R date format and creates
#' the CCAMLR month (Dec=1, ... Nov=12)
#' @param data CCAMLR data extract
#' @param column name of the field to convert to a date
#' @param prefix a prefix to be added to the new field names
#' @param ... additional arguments
#' @export
process_date <- function(data, column, prefix = NULL, ...){
  if(!is.null(prefix)) prefix <- paste0(prefix, "_")
  ## take the data and create a date field from the specified column
  data[paste0(prefix, "Date")] <- as.Date(data[[column]],"%Y-%m-%d %H:%M:%S",tz="UTC")
  data[paste0(prefix, "Month_full")] <- months(data[[paste0(prefix, "Date")]], abbreviate = TRUE)
  ## http://stackoverflow.com/questions/6549239/convert-mmm-to-numeric-in-r
  data[paste0(prefix, "Month")] <- match(data[[paste0(prefix, "Month_full")]], month.abb)
  data[paste0(prefix, "CCAMLR_Month")] <- data[[paste0(prefix, "Month")]] %% 12 + 1
  ## return the data
  data
}

#' Process CCAMLR csv data extract
#'
#' Process CCAMLR csv data extract for use in Chapman estimate of biomass
#'
#' @param data CCAMLR data extract
#' @param location either a ASD code or a research block (must be character format)
#' @param species either "TOA" or "TOP"
#' @param select fields to select in the data (default = NULL selects all fields)
#' @param seasons vector of CCAMLR fishing seasons
#' @param ... additional arguments
#' @aliases process_releases process_recaptures
#' @export
process_catch <- function(data, location, species, seasons, select=NULL, ...){
  ## some checks
  if(length(location) > 1) stop("only one location may currently be specified")
  if(length(species) > 1) stop("only one species may currently be specified")
  if(!species %in% c("TOA", "TOP")) stop("species must be either 'TOA' or 'TOP'")
  ## if no fields specified select columns
  if(is.null(select)) select <- names(data)
  ## extract either a RB or ASD depending on how location is specified
  if(is.integer0(grep("_", location))){
    ## if no '_' then location is an ASD_CODE
    d <- data[data[["SPECIES_CODE"]] %in% species &
                data[["ASD_CODE"]] %in% location, select]
  }else if(length(grep("_", location)==1)){
    ## if 1 and only 1 '_' then location is a RESEARCH_BLOCK
    d <- data[data[["SPECIES_CODE"]] %in% species &
                data[["RESEARCH_BLOCK_CODE_START_SET"]] %in% location, select]
  }else stop("Incorrect location format specified")
  ## create an R date field
  d <- process_date(d, column="SET_START_DATE")
  ## return the data
  d
}

#' @export
#' @rdname process_catch
process_releases <- function(data, location, species, seasons, select=NULL, ...){
  ## some checks
  if(length(location) > 1) stop("only one location may currently be specified")
  if(length(species) > 1) stop("only one species may currently be specified")
  if(!species %in% c("TOA", "TOP")) stop("species must be either 'TOA' or 'TOP'")
  ## if no fields specified select columns
  if(is.null(select)) select <- names(data)
  ## extract either a RB or ASD depending on how location is specified
  if(is.integer0(grep("_", location))){
    ## if no '_' then location is an ASD_CODE
    d <- data[data[["SPECIES_CODE"]] %in% species &
                data[["ASD_CODE"]] %in% location, select]
  }else if(length(grep("_", location)==1)){
    ## if 1 and only 1 _ then location is a RESEARCH_BLOCK
    d <- data[data[["SPECIES_CODE"]] %in% species &
                data[["RESEARCH_BLOCK_CODE"]] %in% location, select]
  }else stop("Incorrect location format specified")
  ## create an R date field
  d <- process_date(d, column="DATE_TAGGED")
  ## return the data
  d
}

#' @export
#' @rdname process_catch
process_recaptures <- function(data, location, species, select=NULL, ...){
  ## some checks
  if(length(location) > 1) stop("only one location may currently be specified")
  if(length(species) > 1) stop("only one species may currently be specified")
  if(!species %in% c("TOA", "TOP")) stop("species must be either 'TOA' or 'TOP'")
  ## if no fields specified select columns
  if(is.null(select)) select <- names(data)
  ## extract either a RB or ASD depending on how location is specified
  if(is.integer0(grep("_", location))){
    ## if no '_' then location is an ASD_CODE
    d <- data[data[["SPECIES_CODE_RECAPTURE"]] %in% species &
                data[["ASD_CODE_RELEASE"]] %in% location &
                data[["ASD_CODE_RECAPTURE"]] %in% location, select]
  }else if(length(grep("_", location)==1)){
    ## if 1 and only 1 _ then location is a RESEARCH_BLOCK
    d <- data[data[["SPECIES_CODE_RECAPTURE"]] %in% species &
                data[["RESEARCH_BLOCK_CODE_RELEASE"]] %in% location &
                data[["RESEARCH_BLOCK_CODE_RECAPTURE"]] %in% location, select]
  }else stop("Incorrect location format specified")
  ## create date fields for release and recapture
  d <- process_date(d, column="DATE_TAGGED", prefix="Release")
  d <- process_date(d, column="DATE_RECAPTURED", prefix="Recapture")
  ## return the data
  d
}

#' Extract releases
#'
#' Extract release data for calculation of biomass
#' @param data recapture data with fields SEASON and CCAMLR_Month
#' @param seasons vector of seasons
#' @param ... additional arguments
#' @export
extract_releases <- function(data, seasons, ...){
  ## define the number of months
  n_months <- 12 #* could drop months
  ## define a matrix to store the recaptures
  rels <- matrix(0, length(seasons), n_months)
  rownames(rels) <- seasons
  ## loop to fill the matrix
  for(i in 1:length(seasons)){
    for(j in 1:n_months){
      rels[i, j] <- nrow(data[data[["SEASON"]] == seasons[i] &
                                data[["CCAMLR_Month"]] == j,])
    }
  }
  #* consider adding a class
  ## return the matrix of releases
  rels
}

#' Extract recaptures Season
#'
#' Extract recapture data for calculation of biomass for a CCAMLR Season
#' @param data tag release and recapture data with fields SEASON_RELEASE,
#' SEASON_RECAPTURE 
#' @param rel_seasons vector of tag release seasons
#' @param ... additional arguments
#' @importFrom plyr ddply
#' @export
extract_recaptures_season <- function(data, rel_seasons){
  ## define an array to store recaps by season and month of release and recapture
  # subtract the first year of releases as we dont want to include within season recaptures 
  # sort release seasons
  Release_data <- data$Releases[data$Releases[["SEASON"]]%in%rel_seasons,]
  Recapture_data <- data$Recaptures[data$Recaptures[["SEASON_RECAPTURE"]]%in%rel_seasons,]
  # remove within seasons recaps
  Recapture_data <- Recapture_data[Recapture_data[["SEASON_RECAPTURE"]]!=Recapture_data[["SEASON_RELEASE"]],]
  
  rel_seasons <- sort(rel_seasons)
  tag_data <- matrix(0, length(rel_seasons),length(rel_seasons)+2)
  tag_data <- data.frame(tag_data)
  names(tag_data)<-c("Year","Releases",rel_seasons)
  tag_data$Year <- rel_seasons 
  Releases <- ddply(Release_data,.(SEASON),nrow)
  tag_data$Releases[tag_data$Year%in%Releases$SEASON]<- Releases$V1
  
  ## loop to fill the array
  # for(i in 1:length(recap_seasons)){
  for(k in 1:length(rel_seasons)){
    recaps <- ddply(Recapture_data[Recapture_data[["SEASON_RELEASE"]] == rel_seasons[k],],.(SEASON_RECAPTURE),nrow)
    tag_data[k,names(tag_data)%in%recaps$SEASON_RECAPTURE] <- recaps$V1 
    
    #   removes within season recaps but dont think you need it 
    #data[["SEASON_RECAPTURE"]]!=rel_seasons[k]
  }
  # }
  
  #* consider adding a class
  ## return the array of recaptures
  # turn into dataframe
  tag_data
}






#' Extract recaptures
#'
#' Extract recapture data for calculation of biomass
#' @param data tag release and recapture data with fields SEASON_RELEASE,
#' SEASON_RECAPTURE and Recapture_CCAMLR_Month
#' @param rel_seasons vector of tag release seasons
#' @param recap_seasons vector of tag recapture seasons
#' @param ... additional arguments
#' @export
extract_recaptures <- function(data, rel_seasons, recap_seasons, ...){
  ## define the number of months
  n_months <- 12
  ## define an array to store recaps by season and month of release and recapture
  recaps <- array(0, dim=c(length(rel_seasons), n_months, n_months,
                           length(recap_seasons)))
  #* lucy added dimnames to the array 
  dimnames(recaps) <- c("release_season","release_month","recapture_month","recapture_season")
  ## loop to fill the array
  for(i in 1:length(recap_seasons)){
    for(j in 1:n_months){
      for(k in 1:length(rel_seasons)){
        for(l in 1:n_months){
          recaps[k, l, j, i] <- nrow(data[data[["SEASON_RELEASE"]] == rel_seasons[k] &
                                            data[["Release_CCAMLR_Month"]] == l &
                                            data[["SEASON_RECAPTURE"]] == recap_seasons[i] &
                                            data[["Recapture_CCAMLR_Month"]] == j,])
        }
      }
    }
  }
  #* consider adding a class
  ## return the array of recaptures
  recaps
}


#' Extract haul data 
#'
#' Extract recapture data for calculation of biomass
#' @param data catch and tag recapture data
#' SEASON_RECAPTURE and Recapture_CCAMLR_Month
#' @param rel_seasons vector of tag release seasons
#' @param measure "numbers" if fish numbers are required or "weights" if fish weights are required
#' @import plyr reshape2 
#' @export
extract_catch_data <- function(data, rel_seasons,measure){
  ## define an array to store recaps by season and month of release and recapture
  # subtract the first year of releases as we dont want to include within season recaptures 
  # sort release seasons
  Catch_data <- data$Catch[data$Catch[["Season"]]%in%rel_seasons[length(rel_seasons)],]
  Recapture_data <- data$Recaptures[data$Recaptures[["SEASON_RECAPTURE"]]%in%rel_seasons[length(rel_seasons)],]
  # remove within seasons recaps
  Recapture_data <- Recapture_data[Recapture_data[["SEASON_RECAPTURE"]]!=Recapture_data[["SEASON_RELEASE"]],]
  # count recaptures per haul
  N_recaps_haul_by_haul=ddply(Recapture_data,.(CRUISE_ID_RECAPTURE,SET_ID_RECAPTURE,SPECIES_CODE_RECAPTURE,SEASON_RELEASE),nrow)
  
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="V1"]="N_recaptures"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="CRUISE_ID_RECAPTURE"]="CRUISE_ID"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="SET_ID_RECAPTURE"]="SET_ID"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="SPECIES_CODE_RECAPTURE"]="SPECIES_CODE"
  catch_recap_data=join(Catch_data,N_recaps_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
  
  switch(measure,
    numbers = catch_recap_data <-subset(catch_recap_data,select=c(CAUGHT_N_TOTAL,SEASON_RELEASE,N_recaptures,CRUISE_ID,SET_ID,SPECIES_CODE)),
    weights = catch_recap_data <-subset(catch_recap_data,select=c(CAUGHT_KG_TOTAL,SEASON_RELEASE,N_recaptures,CRUISE_ID,SET_ID,SPECIES_CODE))
  )
  # reshape to make each release season a column 
  catch_recap_data <- dcast(catch_recap_data,CRUISE_ID + SET_ID + SPECIES_CODE+ CAUGHT_N_TOTAL ~ SEASON_RELEASE,value.var ="N_recaptures")
  catch_recap_data<-catch_recap_data[,!names(catch_recap_data)%in%c("NA","CRUISE_ID","SET_ID","SPECIES_CODE")]
  # replace NA N_recapture values with zero
  catch_recap_data[is.na(catch_recap_data)]<- 0
  catch_recap_output<-data.frame(matrix(0,nrow=nrow(Catch_data),ncol=length(rel_seasons)+1))
  names(catch_recap_output)<-c(names(catch_recap_data)[1],rel_seasons)
  catch_recap_output[,names(catch_recap_output)%in%names(catch_recap_data)]<-catch_recap_data
  
  catch_recap_output
}


#' Process data for the multiple release function
#'
#' Wrapper function over the three datasets, returns a list of three dataframes
#' @param catch_data CCAMLR C2 data extract
#' @param release_data CCAMLR tag release data extract
#' @param recapture_data CCAMLR tag release and recapture data extract
#' @param location either a ASD code or a research block (must be character format)
#' @param species either "TOA" or "TOP"
#' @param select_catch catch fields to select
#' @param select_releases tag release fields to select
#' @param select_recaptures tag recapture fields to select
#' @param ... additional arguments
#' @export
process_tag_data <- function(catch_data, release_data, recapture_data, location, species,
                             select_catch=NULL, select_releases=NULL,select_recaptures=NULL,   ...){
  ## process the catch data
  catch <- process_catch(catch_data, location, species, select_catch)
  ## process the tag release data
  releases <- process_releases(release_data, location, species, select_releases)
  ## process the tag recapture data
  recaptures <- process_recaptures(recapture_data, location, species, select_recaptures)
  ## bundle into a list
  obj <- list("Catch" = catch,
              "Releases" = releases,
              "Recaptures" = recaptures)
  ## return the object
  obj
}
