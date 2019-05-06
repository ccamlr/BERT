#' Extract and reformat release and recapture data 
#'
#' Extract and reformat recapture data for tag based biomass estimation
#' @param release_data a single column data frame with the CCAMLR fishing season of individual fish release events
#' @param recapture_data a two column data frame with the CCAMLR fishing season of release in the first column with the matched CCAMLR fishing season of recapture in the second column for each individual tagged fish. 
#' @param release_seasons vector of seasons of interest where the default = NULL uses release_data seasons included in release_data dataframe
#' @param recapture_season a single season that should match the catch season if applying a tag based estimate using the CCAMLR method where the default = NULL
#' @param within_season_recaps is TRUE to include within season recaptures or FALSE to exclude them with default=FALSE 
#' @importFrom plyr join
#' @export
#' 
extract_recaptures_season <- function(release_data, recapture_data,release_seasons=NULL,recapture_season=NULL, within_season_recaps=FALSE){
  # remove within seasons recaps
  if(!isTRUE(within_season_recaps)){
    recapture_data <- matrix(recapture_data[recapture_data[,1]!=recapture_data[,2],],ncol = 2)
  }
  # remove release and recapture data from unwanted releases seasons 
  if(!is.null(release_seasons)){
    release_data <- release_data[release_data%in%release_seasons]
    recapture_data <- matrix(recapture_data[recapture_data[,1]%in%release_seasons,],ncol=2)
  }
  if(!is.null(recapture_season)){
    recapture_data <- matrix(recapture_data[recapture_data[,2]%in%recapture_season,],ncol=2)
  }
  
  # setup data.frame for output where release season defines a matrix
  # where nrows include the number of releases for each year and the number of columns include recaptures are summed 
  # for a release event in each season
  
  
  if(is.null(release_seasons)){
    release_seasons <- sort(unique(release_data))
  }else{
    release_seasons <- sort(release_seasons)
  }
  
  # if(is.null(recapture_seasons)){
  #   recapture_seasons <- sort(unique(recapture_data[,2]))
  # }else{
  #   release_seasons <- sort(recapture_seasons)
  # }
  
  tag_data <- matrix(0, length(release_seasons),length(release_seasons)+2)
  tag_data <- data.frame(tag_data)
  names(tag_data)<-c("Year","Releases",release_seasons)
  tag_data$Year <- release_seasons 
  # need to check variable names from table
  Releases <- data.frame(table(release_data))
  
  tag_data$Releases[tag_data$Year%in%Releases[,1]]<- Releases[,2]
  
  ## loop to fill the array
  for(k in 1:length(release_seasons)){
    recaps <- recapture_data[recapture_data[,1]==release_seasons[k],2]
    recaps <- data.frame(table(recaps))
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
#' Extract catch data for cpue by seabed area biomass (KR August 2018 updated version)
#' @param catch_data catch data at haul-by-haul resolution that includes a data frame with columns of data in the following order: Unique Catch ID record reference, Fishing Season,cruise id, set id, species code, retained catch quantity (kg),long line length 
#' @param release_data release data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: Fishing Season,cruise id, set id, species code and fish weights if available and measure="weights", if fish weights are not available and measure="counts" these weights are estimated 
#' @param catch_season is the CCAMLR fishing seasons of interest where the default = NULL uses all the catch data seasons included in the catch_data dataframe
#' @param measure "counts" if only fish counts are available or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish to estimate the fish release weights per haul if measure= "counts" otherwise the default=NULL
#' @param target_species is the species code for the species that is targetted in the research block or reference area
#' @importFrom plyr ddply join
#' @importFrom reshape2 dcast 
#' @export
#' 
extract_catch_data_cpue_est<-function (catch_data, release_data, catch_season = NULL, measure, 
                                       mean_fish_weight = NULL, target_species) {
  
  
  names(catch_data) <- c("ID", "SEASON", "CRUISE_ID", "SET_ID", 
                         "SPECIES_CODE", "CAUGHT_KG_TOTAL", "LINE_LENGTH")
  if (measure == "counts") {
    release_data$MEASURE <- rep(1, nrow(release_data))
  }
  names(release_data) <- c("SEASON", "CRUISE_ID", "SET_ID", 
                           "SPECIES_CODE", "MEASURE")
  if (ncol(catch_data) != 7) 
    stop("catch_data table should have seven columns check that all necessary information has been provided")
  if ((ncol(release_data) != 5)) 
    stop("release_data table should have five columns, check all necessary information has been provided")
  if (measure == "counts" & is.null(mean_fish_weight)) 
    stop("a mean fish weight has not been entered, but is required for fish count measurements")
  if (!is.null(catch_season) & nrow(catch_data) > 0) {
    catch_data <- catch_data[catch_data$SEASON %in% catch_season, 
                             ]
  }
  if (!is.null(catch_season) & nrow(release_data) > 0) {
    release_data <- release_data[release_data$SEASON %in% 
                                   catch_season, ]
  }
  if (nrow(release_data) > 0) {
    Weight_releases_haul_by_haul <- stats::aggregate(MEASURE ~ 
                                                       CRUISE_ID + SET_ID + SPECIES_CODE + SEASON, data = release_data, 
                                                     sum)
    names(Weight_releases_haul_by_haul) <- c("CRUISE_ID", 
                                             "SET_ID", "SPECIES_CODE", "SEASON", "RELEASE_TOTAL")
    catch_release_data <- plyr::join(catch_data, Weight_releases_haul_by_haul, 
                                     by = c("CRUISE_ID", "SET_ID", "SPECIES_CODE"))
    catch_release_data$RELEASE_TOTAL[is.na(catch_release_data$RELEASE_TOTAL)] <- 0
    catch_release_data$CAUGHT_KG_TOTAL[is.na(catch_release_data$CAUGHT_KG_TOTAL)] <- 0
    if (measure == "counts") {
      catch_release_data$CAUGHT_KG_TOTAL <- catch_release_data$CAUGHT_KG_TOTAL + 
        (catch_release_data$RELEASE_TOTAL * mean_fish_weight)
    }
    else {
      catch_release_data$CAUGHT_KG_TOTAL <- catch_release_data$CAUGHT_KG_TOTAL + 
        catch_release_data$RELEASE_TOTAL
    }
  }
  else {
    catch_release_data <- catch_data
  }
  catch_release_data$CAUGHT_KG_KM <- catch_release_data$CAUGHT_KG_TOTAL/(catch_release_data$LINE_LENGTH/1000)
  catch_release_data$CAUGHT_KG_KM[!catch_release_data$SPECIES_CODE %in% 
                                    target_species] <- 0
  
  
  catch_release_data <- catch_release_data[catch_release_data$SPECIES_CODE %in% 
                                             target_species, ]
  catch_release_data$CAUGHT_KG_KM
}

#' Extract haul data 
#'
#' Extract catch data for cpue by seabed area biomass
#' @param catch_data catch data at haul-by-haul resolution that includes a data frame with columns of data in the following order: Unique Catch ID record reference, fishing season,cruise id, set id, species code, catch quantity 
#' @param release_data release data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: fishing season,cruise id, set id, species code and fish weights if available and measure="weights", if fish weights are not available and measure="counts" these weights are estimated based on the mean_fish_weight provided
#' @param recapture_data recapture data at the haul-by-haul resolution that includes a data frame with columns of data in the following order: fishing season of release, fishing season of recapture, cruise id, set id and species code 
#' @param release_seasons CCAMLR fishing seasons for which tagged fish releases should be included. Default=NULL 
#' @param catch_season CCAMLR fishing season for  which catch and recaptures should be limited to. Default=NULL 
#' @param measure "counts" if fish counts are required or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish (kg) to estimate the fish release weights per haul
#' @param target_species is the species that is targetted in the research block or reference area
#' @importFrom plyr join
#' @importFrom reshape2 dcast
#' @export
#' 
extract_catch_data_tag_est <- function(catch_data, release_data,recapture_data,release_seasons=NULL,catch_season=NULL,measure,mean_fish_weight=NULL,target_species){
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
  
  # remove release seasons that are not required if specified and if seasons are not specified then
  # specify as unique seasons in release data 
  # remember that only weight data of released fish in the current year is used here 
  
  if(!is.null(release_seasons)){
    recapture_data <- recapture_data[recapture_data$SEASON_RELEASE%in%release_seasons,]
  }else{
    release_seasons <- unique(release_data$SEASON)
  }
  
  # remove catch and recapture seasons that are not required if specified and if seasons are not specified then
  # specify as unique seasons in release data 
  if(!is.null(catch_season)){
    recapture_data <- recapture_data[recapture_data$SEASON_RECAPTURE%in%catch_season,]
    catch_data <- catch_data[catch_data$SEASON%in%catch_season,]
    release_data <- release_data[release_data$SEASON%in%catch_season,]
  }
  
  # remove within season recap
  recapture_data <- recapture_data[recapture_data$SEASON_RECAPTURE!=recapture_data$SEASON_RELEASE,]
  
  # count recaptures per haul # this might need to be aggregated by SEASON_RELEASE instead of SEASON_RECAPTURE 
  # because at the end I think the recaptures for each release cohort are what goes into the multi_release function
  # N_recaps_haul_by_haul<- plyr::ddply(recapture_data,.(recapture_data$CRUISE_ID,recapture_data$SET_ID,recapture_data$SPECIES_CODE,recapture_data$SEASON_RELEASE),function(x){nrow(x)})
  # N_recaps_haul_by_haul<- plyr::ddply(recapture_data,.(recapture_data$CRUISE_ID,recapture_data$SET_ID,recapture_data$SPECIES_CODE,recapture_data$SEASON_RELEASE),summarise,nrow)
  # 
  # N_recaps_haul_by_haul<- stats::aggregate(recapture_data,by=list(CRUISE_ID,SET_ID,SPECIES_CODE,SEASON_RELEASE),FUN=length)
  # N_recaps_haul_by_haul_2<- stats::aggregate(SEASON_RECAPTURE ~ CRUISE_ID+SET_ID+SPECIES_CODE+SEASON_RELEASE,data=recapture_data,function(x){nrow(x)})
  N_recaps_haul_by_haul<- stats::aggregate(SEASON_RECAPTURE ~ CRUISE_ID+SET_ID+SPECIES_CODE+SEASON_RELEASE,data=recapture_data,length)
  
  
  names(N_recaps_haul_by_haul)<-c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON_RELEASE","N_RECAPTURES")
  
  catch_recap_data<- plyr::join(catch_data,N_recaps_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
  
  # sum estimated weight of all fish released per haul
  # Weight_releases_haul_by_haul<- ddply(release_data, .(release_data$CRUISE_ID,release_data$SET_ID,release_data$SPECIES_CODE,release_data$SEASON),
  # function(x){sum(x$MEASURE)})
  # Weight_releases_haul_by_haul<- ddply(release_data, .(release_data$CRUISE_ID,release_data$SET_ID,release_data$SPECIES_CODE,release_data$SEASON),
  #                                      function(x){sum(x$MEASURE)})
  
  if(nrow(release_data)>0){
    Weight_releases_haul_by_haul<- stats::aggregate(MEASURE~CRUISE_ID+SET_ID+SPECIES_CODE+SEASON,data=release_data,sum)
    
    names(Weight_releases_haul_by_haul)<-c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON","RELEASE_TOTAL")
    
    catch_recap_release_data<- plyr::join(catch_recap_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
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
    
    catch_recap_release_data$N_RECAPTURES[is.na(catch_recap_release_data$N_RECAPTURES)]<-0
    
    catch_recap_release_data$CAUGHT_KG_TOTAL[is.na(catch_recap_release_data$CAUGHT_KG_TOTAL)]<-0
  }else{
    
    catch_recap_release_data <- catch_recap_data
    
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
  catch_recap_data <- catch_recap_release_data[,names(catch_recap_release_data)%in%c("CAUGHT_KG_TOTAL","SEASON_RELEASE","N_RECAPTURES","CRUISE_ID","SET_ID","SPECIES_CODE")]
  # reshape to make each release season a column 
  catch_recap_data <-dcast(catch_recap_data,CRUISE_ID + SET_ID + SPECIES_CODE + CAUGHT_KG_TOTAL ~ SEASON_RELEASE,value.var ="N_RECAPTURES",drop=TRUE,fun.aggregate=sum)
  
  # remove var names that are not required in output
  catch_recap_data<-catch_recap_data[,!names(catch_recap_data)%in%c("NA","CRUISE_ID","SET_ID","SPECIES_CODE")]
  # replace NA N_recapture values with zero
  catch_recap_data[is.na(catch_recap_data)]<- 0
  # not sure if this part is really necessary - check further
  catch_recap_output<-data.frame(matrix(0,nrow=nrow(catch_recap_data),ncol=length(release_seasons)+1))
  names(catch_recap_output)<-c(names(catch_recap_data)[1],release_seasons)
  catch_recap_output[,names(catch_recap_output)%in%names(catch_recap_data)]<-catch_recap_data
  
  catch_recap_output
}

