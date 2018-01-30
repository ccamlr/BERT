#' Extract and reformat release and recapture data 
#'
#' Extract and reformat recapture data for tag based biomass estimation
#' @param release_data A single column data.frame with the season of individual fish release events
#' @param recapture_data A two column data.frame with the season of release with the matched season of recapture for individual tagged fish. The first column includes the season of release and the second column the season of recapture
#' @param rel_seasons vector of tag release seasons to restrict the extract too with default = NULL 
#' @param within_season_recaps is TRUE to include within season recaptures or FALSE to exclude them with default=FALSE 
#' @importFrom plyr ddply
#' @export
#' 
extract_recaptures_season <- function(release_data, recapture_data,rel_seasons=NULL,within_season_recaps=FALSE){
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
#' @param data catch and tag releases are matched at the haul-by-haul level
#' @param catch_seasons vector of seasons for which catch data exists
#' @param measure "numbers" if fish numbers are required or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish to estimate the fish release weights per haul from
#' @param target_species is the species that is targetted in the research block or reference area
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast 
#' @export
#' 
extract_catch_data_cpue_est <- function(data,catch_seasons,measure,mean_fish_weight,target_species){
  ## define an array to store recaps by season and month of release and recapture
  # subtract the first year of releases as we dont want to include within season recaptures 
  # sort release seasons
  Catch_data <- data$Catch[data$Catch[["Season"]]%in%catch_seasons,]
  
  Release_data <-data$Releases[data$Releases[["SEASON"]]%in%catch_seasons,]
  
  switch(measure,
         numbers = {
           # sum estimated weight of all fish released per haul
           Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,SEASON),nrow)
           names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="N_released"
           
           catch_release_data=join(Catch_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
           # assume NA values Total_kg_released values are zero
           catch_release_data$N_released[is.na(catch_release_data$N_released)]=0
           
           # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors 
           catch_release_data$CAUGHT_N_TOTAL[is.na(catch__release_data$CAUGHT_N_TOTAL)]=0
           
           catch_release_data$CAUGHT_N_TOTAL=catch_release_data$CAUGHT_N_TOTAL+catch_release_data$N_released
           
           # catch_release_data <-subset(catch_release_data,select=c(CAUGHT_N_TOTAL,CRUISE_ID,SET_ID,SPECIES_CODE))
           # catch_release_data<-subset(catch_release_data,select=CAUGHT_N_TOTAL)
         },
         weights = {
           if(any(names(Release_data)%in%"EST_WEIGHT_KG")){
             if(nrow(Release_data)>0){
               Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,SEASON),
                                                  function(x){sum(x$EST_WEIGHT_KG)})
               
               names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="Total_kg_released"
               catch_release_data=join(Catch_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
               # assume NA values Total_kg_released values are zero
               catch_release_data$Total_kg_released[is.na(catch_release_data$Total_kg_released)]=0
               
               # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors
               catch_release_data$CAUGHT_KG_TOTAL[is.na(catch_release_data$CAUGHT_KG_TOTAL)]=0
               
               catch_release_data$CAUGHT_KG_TOTAL=catch_release_data$CAUGHT_KG_TOTAL+catch_release_data$Total_kg_released
             }else{catch_release_data <- Catch_data}
             
             catch_release_data$CAUGHT_KG_KM <- catch_release_data$CAUGHT_KG_TOTAL/(catch_release_data$LINE_LENGTH/1e3)
             
             # catch_release_data <-subset(catch_release_data,select=c(ID,CAUGHT_KG_KM,CRUISE_ID,SET_ID,SPECIES_CODE))
             
             
             # reshape to make each release season a column 
             # catch_release_data<-subset(catch_release_data,select=CAUGHT_KG_KM)
           }else{
             # sum estimated weight of all fish released per haul
             if(nrow(Release_data)>0){
               Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,SEASON),nrow)
               names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="Est_kg_released"
               Weight_releases_haul_by_haul$Est_kg_released<- Weight_releases_haul_by_haul$Est_kg_released*mean_fish_weight
               
               catch_release_data=join(Catch_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
               # assume NA values Total_kg_released values are zero
               catch_release_data$Est_kg_released[is.na(catch_release_data$Est_kg_released)]=0
               
               # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors 
               catch_release_data$CAUGHT_KG_TOTAL[is.na(catch_release_data$CAUGHT_KG_TOTAL)]=0
               
               catch_release_data$CAUGHT_KG_TOTAL=catch_release_data$CAUGHT_KG_TOTAL+catch_release_data$Est_kg_released
             }else{catch_release_data <- Catch_data}
             
             
             catch_release_data$CAUGHT_KG_KM <- catch_release_data$CAUGHT_KG_TOTAL/(catch_release_data$LINE_LENGTH/1e3)
             
             # catch_release_data <-subset(catch_release_data,select=c(CAUGHT_KG_KM,CRUISE_ID,SET_ID,SPECIES_CODE))
             
             
             # reshape to make each release season a column 
             # catch_release_data<-subset(catch_release_data,select=CAUGHT_KG_KM)
           }
         }  
  )
  
  # if SPECIES_CODE catch is not the target species then set the catch to zero 
  catch_release_data$CAUGHT_KG_KM[!catch_release_data$SPECIES_CODE%in%target_species]=0
  
  # remove duplicate sets where both TOP and TOA were caught (i.e. a zero catch record from a non-target species will only count if there was no target species caught)
  # this should be C2.ID as there may be hauls that dont have an observed CRUISE and SET ID
  
  catch_release_data <- catch_release_data[!duplicated(catch_release_data$ID),]
  
  catch_release_data <- subset(catch_release_data,select=CAUGHT_KG_KM)
}

#' Extract haul data 
#'
#' Extract catch data for cpue by seabed area biomass
#' @param data catch and tag release and tag recapture data matched at the haul by-haul level
#' @param rel_seasons vector of release seasons 
#' @param measure "numbers" if fish numbers are required or "weights" if fish weights are required
#' @param mean_fish_weight is the mean weight of a fish to estimate the fish release weights per haul from
#' @param target_species is the species that is targetted in the research block or reference area
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
#' @export
#' 
extract_catch_data_tag_est <- function(data, rel_seasons,measure,mean_fish_weight,target_species){
  ## define an array to store recaps by season and month of release and recapture
  # subtract the first year of releases as we dont want to include within season recaptures 
  # sort release seasons
  Catch_data <- data$Catch[data$Catch[["Season"]]%in%rel_seasons[length(rel_seasons)],]
  Recapture_data <- data$Recaptures[data$Recaptures[["SEASON_RECAPTURE"]]%in%rel_seasons[length(rel_seasons)],]
  # remove within seasons recaps
  Recapture_data <- Recapture_data[Recapture_data[["SEASON_RECAPTURE"]]!=Recapture_data[["SEASON_RELEASE"]],]
  
  # this is only used in calculating susbtantial catch for the current season 
  Release_data <-data$Releases[data$Releases[["SEASON"]]%in%rel_seasons[length(rel_seasons)],]
  # count recaptures per haul
  N_recaps_haul_by_haul=ddply(Recapture_data,.(CRUISE_ID_RECAPTURE,SET_ID_RECAPTURE,SPECIES_CODE_RECAPTURE,SEASON_RELEASE),nrow)
  
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="V1"]="N_recaptures"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="CRUISE_ID_RECAPTURE"]="CRUISE_ID"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="SET_ID_RECAPTURE"]="SET_ID"
  names(N_recaps_haul_by_haul)[names(N_recaps_haul_by_haul)=="SPECIES_CODE_RECAPTURE"]="SPECIES_CODE"
  catch_recap_data=join(Catch_data,N_recaps_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
  
  switch(measure,
         numbers = {
           # sum estimated weight of all fish released per haul
           Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,RESEARCH_BLOCK_CODE,SEASON),nrow)
           names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="N_released"
           
           catch_recap_release_data=join(catch_recap_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
           # assume NA values Total_kg_released values are zero
           catch_recap_release_data$N_released[is.na(catch_recap_release_data$N_released)]=0
           
           # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors 
           catch_recap_release_data$CAUGHT_N_TOTAL[is.na(catch_recap_release_data$CAUGHT_N_TOTAL)]=0
           
           catch_recap_release_data$CAUGHT_N_TOTAL=catch_recap_release_data$CAUGHT_N_TOTAL+catch_recap_release_data$N_released
           
           catch_recap_data <-subset(catch_recap_release_data,select=c(CAUGHT_N_TOTAL,SEASON_RELEASE,N_recaptures,CRUISE_ID,SET_ID,SPECIES_CODE))
           catch_recap_data <- dcast(catch_recap_release_data,CRUISE_ID + SET_ID + SPECIES_CODE+ CAUGHT_N_TOTAL ~ SEASON_RELEASE,value.var ="N_recaptures")
           
         },
         weights = {
           if(any(names(Release_data)%in%"EST_WEIGHT_KG")){
             # 
             # names(catch_recap_data)[names(catch_recap_data)%in%"Season"]="SEASON"
             # catch_recap_release_data=join(catch_recap_data,Release_data,by=c("CRUISE_ID","SET_ID","SPECIES_CODE","SEASON"))
             # catch_recap_release_data=join(catch_recap_data,Release_data,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
             # 
             # sum estimated weight of all fish released per haul
             Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,RESEARCH_BLOCK_CODE,SEASON),
                                                function(x){sum(x$EST_WEIGHT_KG)})
             
             names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="Total_kg_released"
             catch_recap_release_data=join(catch_recap_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
             # assume NA values Total_kg_released values are zero
             catch_recap_release_data$Total_kg_released[is.na(catch_recap_release_data$Total_kg_released)]=0
             
             # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors
             catch_recap_release_data$CAUGHT_KG_TOTAL[is.na(catch_recap_release_data$CAUGHT_KG_TOTAL)]=0
             
             catch_recap_release_data$CAUGHT_KG_TOTAL=catch_recap_release_data$CAUGHT_KG_TOTAL+catch_recap_release_data$Total_kg_released
             
           }else{
             # sum estimated weight of all fish released per haul
             Weight_releases_haul_by_haul=ddply(Release_data,.(CRUISE_ID,SET_ID,SPECIES_CODE,SEASON),nrow)
             names(Weight_releases_haul_by_haul)[names(Weight_releases_haul_by_haul)=="V1"]="Est_kg_released"
             Weight_releases_haul_by_haul$Est_kg_released<- Weight_releases_haul_by_haul$Est_kg_released*mean_fish_weight
             
             catch_recap_release_data=join(catch_recap_data,Weight_releases_haul_by_haul,by=c("CRUISE_ID","SET_ID","SPECIES_CODE"))
             # assume NA values Total_kg_released values are zero
             catch_recap_release_data$Est_kg_released[is.na(catch_recap_release_data$Est_kg_released)]=0
             
             # assume NA value in CAUGHT_KG_TOTAL data are zero, but checking this data with Dave for any potential processing errors 
             catch_recap_release_data$CAUGHT_KG_TOTAL[is.na(catch_recap_release_data$CAUGHT_KG_TOTAL)]=0
             
             catch_recap_release_data$CAUGHT_KG_TOTAL=catch_recap_release_data$CAUGHT_KG_TOTAL+catch_recap_release_data$Est_kg_released
           }
           
           
           # if SPECIES_CODE catch is not the target species then set the catch to zero 
           catch_recap_release_data$CAUGHT_KG_TOTAL[!catch_recap_release_data$SPECIES_CODE%in%target_species]=0
           
           # if SPECIES_CODE recapture is not the target species then set recapture to zero
           catch_recap_release_data$N_recaptures[!catch_recap_release_data$SPECIES_CODE%in%target_species]=0
           
           # remove duplicate sets where both TOP and TOA were caught (i.e. a zero catch record from a non-target species will only count if there was no target species caught)
           # this should be C2.ID as there may be hauls that dont have an observed CRUISE and SET ID
           
           # correction made so that duplicated hauls where the target species was caught but matched with a different release event
           # are not removed before being transposed and included in the Chapman bootstrap calculation (30-09-2017)
           catch_recap_release_data <- catch_recap_release_data[!(!catch_recap_release_data$SPECIES_CODE%in%target_species&duplicated(catch_recap_release_data$ID)),]
           
           
           # remove columns that arent required for input into the Chapman estimate
           catch_recap_data <-subset(catch_recap_release_data,select=c(CAUGHT_KG_TOTAL,SEASON_RELEASE,N_recaptures,CRUISE_ID,SET_ID,SPECIES_CODE))
           # transpose data so recaptures are aligned with each release year by column
           catch_recap_data <-dcast(catch_recap_data,CRUISE_ID + SET_ID + SPECIES_CODE + CAUGHT_KG_TOTAL ~ SEASON_RELEASE,value.var ="N_recaptures")
         }
  )
  
  
  
  # reshape to make each release season a column 
  catch_recap_data<-catch_recap_data[,!names(catch_recap_data)%in%c("NA","CRUISE_ID","SET_ID","SPECIES_CODE")]
  # replace NA N_recapture values with zero
  catch_recap_data[is.na(catch_recap_data)]<- 0
  catch_recap_output<-data.frame(matrix(0,nrow=nrow(catch_recap_data),ncol=length(rel_seasons)+1))
  names(catch_recap_output)<-c(names(catch_recap_data)[1],rel_seasons)
  catch_recap_output[,names(catch_recap_output)%in%names(catch_recap_data)]<-catch_recap_data
  
  catch_recap_output
}

