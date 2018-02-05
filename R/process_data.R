#' Extract and reformat release and recapture data 
#'
#' Extract and reformat recapture data for tag based biomass estimation
#' @param release_data a single column data frame with the CCAMLR fishing season of individual fish release events
#' @param recapture_data a two column data frame with the CCAMLR fishing season of release in the first column with the matched CCAMLR fishing season of recapture in the second column for each individual tagged fish. 
#' @param release_seasons vector of seasons of interest where the default = NULL uses release_data seasons included in release_data dataframe
#' @param within_season_recaps is TRUE to include within season recaptures or FALSE to exclude them with default=FALSE 
#' @importFrom plyr ddply
#' @export
#' 
extract_recaptures_season <- function(release_data, recapture_data,release_seasons=NULL,within_season_recaps=FALSE){
  # remove release and recapture data from unwanted releases seasons 
  if(!is.null(rel_seasons)){
    release_data <- release_data[release_data[,1]%in%rel_seasons,]
    recapture_data <- recapture_data[recapture_data[,2]%in%rel_season,]
  }
  
  # remove within seasons recaps
  if(!isTRUE(within_season_recaps)){
    recapture_data <- recapture_data[recapture_data[,1]!=recapture_data[,2],]
  }
  
  # setup data.frame for output where release season defines a matrix
  # where nrows include the number of releases for each year and the number of columns include recaptures are summed 
  # for a release event in each season
  
  
  if(is.null(rel_seasons)){
    rel_seasons <- sort(unique(release_data))
  }else{
    rel_seasons <- sort(rel_seasons)
  }
  
  tag_data <- matrix(0, length(rel_seasons),length(rel_seasons)+2)
  tag_data <- data.frame(tag_data)
  names(tag_data)<-c("Year","Releases",rel_seasons)
  tag_data$Year <- rel_seasons 
  # need to check variable names from table
  Releases <- data.frame(table(release_data))
  
  tag_data$Releases[tag_data$Year%in%Releases[,1]]<- Releases[,2]
  
  ## loop to fill the array
  for(k in 1:length(rel_seasons)){
    recaps <- data.frame(table(recapture_data[recapture_data[,1]==rel_seasons[k],]))
    # if there were no recaptures for a particular release event 
    
    if(nrow(recaps)>0){
      tag_data[k,names(tag_data)%in%recaps[,1]] <- recaps[,2]}else{
        tag_data[k,names(tag_data)%in%recaps[,1]] <- 0  
      } 
  }
  
  ## return the array of recaptures
  # turn into dataframe
  tag_data
}


#' Extract haul data 
#'
#' Extract catch data for cpue by seabed area biomass
#' @param catch_data catch data at haul-by-haul resolution that includes a data frame with columns of data in the following order: Unique Catch ID record reference, Fishing Season,cruise id, set id, species code, retained catch quantity (kg),long line length 
#' @param release_data release data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: Fishing Season,cruise id, set id, species code and fish weights if available and measure="weights", if fish weights are not available and measure="counts" these weights are estimated 
#' @param catch_seasons vector of seasons of interest where the default = NULL uses all the catch data seasons included in the catch_data dataframe
#' @param measure "counts" if only fish counts are available or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish to estimate the fish release weights per haul if measure= "counts" otherwise the default=NULL
#' @param target_species is the species code for the species that is targetted in the research block or reference area
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast 
#' @export
#' 
extract_catch_data_cpue_est <- function(catch_data,release_data,catch_seasons=NULL,measure,mean_fish_weight=NULL,target_species){
  
  # assign variable names to catch data dataframe
  names(catch_data) <- c("ID","SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","CAUGHT_KG_TOTAL","LINE_LENGTH")
  
  if(measure=="counts"){
    # create dummy quantity filed with replicated 1
    release_data$MEASURE <- rep(1,nrow(release_data))
  }
  # assign variable names to release data dataframe
  names(release_data) <- c("SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","MEASURE")
  
  # set up check for params
  if (ncol(catch_data)!=7)
    stop("catch_data table should have seven columns check that all necessary information has been provided")
  
  if ((ncol(release_data)!=5))
    stop("release_data table should have five columns, check all necessary information has been provided")
  
  if (measure=="counts" & is.null(mean_fish_weight))
    stop("a mean fish weight has not been entered, but is required for fish count measurements")
  
  # remove seasons that are not required 
  if(!is.null(catch_seasons)){
    catch_data <- catch_data[catch_data$SEASON%in%catch_seasons,]
    release_data <- release_data[release_data$SEASON%in%catch_seasons,]
  }
  
  # aggregate release by by haul so their weight can be added to the total catch retained catch
  if(nrow(release_data)>0){
    Weight_releases_haul_by_haul<- plyr::ddply(release_data, .(release_data$CRUISE_ID,release_data$SET_ID,release_data$SPECIES_CODE,release_data$SEASON),function(x){sum(x$MEASURE)})
    names(Weight_releases_haul_by_haul)<-c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON","RELEASE_TOTAL")
    catch_release_data<- plyr::join(catch_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
    # assume NA values Total_kg_released values are zero
    catch_release_data$RELEASE_TOTAL[is.na(catch_release_data$RELEASE_TOTAL)]<-0
    
    catch_release_data$CAUGHT_KG_TOTAL[is.na(catch_release_data$CAUGHT_KG_TOTAL)]<-0
    
  }else{catch_release_data <- catch_data}
  
  # if release data are added as counts then a mean weight is used to estimate their total weight per haul 
  if(measure=="counts"){
    # convert to weight 
    catch_release_data$CAUGHT_KG_TOTAL <- catch_release_data$CAUGHT_KG_TOTAL+(catch_release_data$RELEASE_TOTAL*mean_fish_weight)
  }else{
    catch_release_data$CAUGHT_KG_TOTAL <- catch_release_data$CAUGHT_KG_TOTAL+catch_release_data$RELEASE_TOTAL
  }
  
  catch_release_data$CAUGHT_KG_KM <- catch_release_data$CAUGHT_KG_TOTAL/(catch_release_data$LINE_LENGTH/1e3)
  
  
  # if SPECIES_CODE catch is not the target species then set the catch to zero 
  catch_release_data$CAUGHT_KG_KM[!catch_release_data$SPECIES_CODE%in%target_species]<-0
  
  # remove duplicate sets where both TOP and TOA were caught (i.e. a zero catch record from a non-target species will only count if there was no target species caught)
  # this should be C2.ID as there may be hauls that dont have an observed CRUISE and SET ID
  
  catch_release_data <- catch_release_data[!duplicated(catch_release_data$ID),]
  
  catch_release_data$CAUGHT_KG_KM
}

#' Extract haul data 
#'
#' Extract catch data for cpue by seabed area biomass
#' @param catch_data catch data at haul-by-haul resolution that includes a data frame with columns of data in the following order: Unique Catch ID record reference, fishing season,cruise id, set id, species code, catch quantity 
#' @param release_data release data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: fishing season,cruise id, set id, species code and fish weights if available and measure="weights", if fish weights are not available and measure="counts" these weights are estimated based on the mean_fish_weight provided
#' @param recapture_data recapture data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: fishing season of release, fishing season of recapture, cruise id, set id and species code 
#' @param season season for which you have catch and recapture data
#' @param measure "counts" if fish counts are required or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish (kg) to estimate the fish release weights per haul from
#' @param target_species is the species that is targetted in the research block or reference area
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
#' @export
#' 
extract_catch_data_tag_est <- function(catch_data, release_data,recapture_data,season=NULL,measure,mean_fish_weight=NULL,target_species){
  ## define an array to store recaps by season and month of release and recapture
  # subtract the first year of releases as we dont want to include within season recaptures 
  # sort release seasons
  # assign variable names to catch data dataframe
  names(catch_data) <- c("ID","SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","CAUGHT_KG_TOTAL")
  
  if(measure=="counts"){
    # create dummy quantity filed with replicated 1
    release_data$MEASURE <- rep(1,nrow(release_data))
  }
  # assign variable names to release dataframe
  names(release_data) <- c("SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","MEASURE")
  
  # assign variables to recapture dataframe
  names(recapture_data) <- c("SEASON_RELEASE","SEASON_RECAPTURE","CRUISE_ID","SET_ID","SPECIES_CODE")
  
  # set up check for params
  if (ncol(catch_data)!=6)
    stop("catch_data table should have seven columns check that all necessary information has been provided")
  
  if ((ncol(release_data)!=5))
    stop("release_data table should have five columns, check all necessary information has been provided")
  
  if ((ncol(recapture_data)!=5))
    stop("recapture_data table should have five columns, check all necessary information has been provided")
  
  if (measure=="counts" & is.null(mean_fish_weight))
    stop("a mean fish weight has not been entered, but is required for fish count measurements")
  
  # if season is null then check that input data is only a single season   
  if(is.null(season)){
    if(length(unique(catch_data$SEASON))|length(unique(release_data$SEASON))|length(unique(recapture_data$SEASON_RECAPTURE))>1){
      stop("more than one fishing season of data has been entered") 
    }
    if(unique(catch_data$SEASON)!=unique(release_data$SEASON)|unique(catch_data$SEASON)!=unique(recapture_data$SEASON_RECAPTURE)|unqiue(release_data$SEASON)!=unique(recapture_data$SEASON_RECAPTURE)){
      stop("the season of retained catch, releases and tagged fish recaptures are not equal")  
    }
  }
  
  
  # remove seasons that are not required 
  if(!is.null(season)){
    if(length(season)>1){
      stop("more than one fishing season of data has been entered") 
    }else{
      catch_data <- catch_data[catch_data$SEASON%in%season,]
      release_data <- release_data[release_data$SEASON%in%season,]
      recapture_data <- recapture_data[recapture_data$SEASON_RECAPTURE%in%season,]}
  }
  
  
  # remove within season recap
  recapture_data <- recapture_data[recapture_data$SEASON_RECAPTURE!=recapture_data$SEASON_RELEASE,]
  
  # count recaptures per haul # this might need to be aggregated by SEASON_RELEASE instead of SEASON_RECAPTURE 
  # because at the end I think the recaptures for each release cohort are what goes into the multi_release function
  N_recaps_haul_by_haul<- plyr::ddply(recapture_data, .(recapture_data$CRUISE_ID,recapture_data$SET_ID,recapture_data$SPECIES_CODE,recapture_data$SEASON_RELEASE),nrow)
  names(N_recaps_haul_by_haul)<- c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON_RELEASE","N_RECAPTURES")
  catch_recap_data<-join(catch_data,N_recaps_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
  
  # sum estimated weight of all fish released per haul
  Weight_releases_haul_by_haul<- ddply(release_data,.(release_data$CRUISE_ID,release_data$SET_ID,release_data$SPECIES_CODE,release_data$SEASON),
                                       function(x){sum(x$MEASURE)})
  names(Weight_releases_haul_by_haul)<-c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON","RELEASE_TOTAL")
  catch_recap_release_data<- join(catch_recap_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
  # assume NA values Total_kg_released values are zero
  catch_recap_release_data$RELEASE_TOTAL[is.na(catch_recap_release_data$RELEASE_TOTAL)]<- 0
  
  # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors
  catch_recap_release_data$CAUGHT_KG_TOTAL[is.na(catch_recap_release_data$CAUGHT_KG_TOTAL)]<- 0
  
  # if release data are added as counts then a mean weight is used to estimate their total weight per haul 
  if(measure=="counts"){
    # convert to weight 
    catch_recap_release_data$CAUGHT_KG_TOTAL <- catch_recap_release_data$CAUGHT_KG_TOTAL+(catch_recap_release_data$RELEASE_TOTAL*mean_fish_weight)
  }else{
    catch_recap_release_data$CAUGHT_KG_TOTAL <- catch_recap_release_data$CAUGHT_KG_TOTAL+catch_recap_release_data$RELEASE_TOTAL
  }
  
  # if SPECIES_CODE catch is not the target species then set the catch to zero 
  catch_recap_release_data$CAUGHT_KG_TOTAL[!catch_recap_release_data$SPECIES_CODE%in%target_species]<-0
  
  # if SPECIES_CODE recapture is not the target species then set recapture to zero
  catch_recap_release_data$N_RECAPTURES[!catch_recap_release_data$SPECIES_CODE%in%target_species]<-0
  
  # identify duplicates 
  duplicate_hauls <- catch_recap_release_data[duplicated(catch_recap_release_data$ID)|duplicated(catch_recap_release_data$ID, fromLast = TRUE),]

  # remove them from the orginal dataframe before selecting target species from duplicates 
  catch_recap_release_data <- catch_recap_release_data[!(duplicated(catch_recap_release_data$ID)|duplicated(catch_recap_release_data$ID, fromLast = TRUE)),]
  
  # select target species from duplicate hauls 
  duplicate_hauls_target <-  duplicate_hauls[duplicate_hauls$SPECIES_CODE%in%target_species,]
  
  # add target species duplicate hauls back into dataframe
  catch_recap_release_data <- rbind(catch_recap_release_data, duplicate_hauls_target)
  
 
  # remove columns that arent required for input into the Chapman estimate
  catch_recap_data <-subset(catch_recap_release_data,select=c(CAUGHT_KG_TOTAL,SEASON_RELEASE,N_RECAPTURES,CRUISE_ID,SET_ID,SPECIES_CODE))
  # transpose data so recaptures are aligned with each release year by column
  catch_recap_data <-dcast(catch_recap_data,CRUISE_ID + SET_ID + SPECIES_CODE + CAUGHT_KG_TOTAL ~ SEASON_RELEASE,value.var ="N_RECAPTURES")
  
  
  # reshape to make each release season a column 
  catch_recap_data<-catch_recap_data[,!names(catch_recap_data)%in%c("NA","CRUISE_ID","SET_ID","SPECIES_CODE")]
  # replace NA N_recapture values with zero
  catch_recap_data[is.na(catch_recap_data)]<- 0
  catch_recap_output<-data.frame(matrix(0,nrow=nrow(catch_recap_data),ncol=length(rel_seasons)+1))
  names(catch_recap_output)<-c(names(catch_recap_data)[1],rel_seasons)
  catch_recap_output[,names(catch_recap_output)%in%names(catch_recap_data)]<-catch_recap_data
  
  catch_recap_output
}

