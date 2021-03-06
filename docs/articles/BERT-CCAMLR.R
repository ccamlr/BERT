## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("ccamlr/BERT",build_vignettes=TRUE,auth_token="7ef7614738c2b3463fc791d4f22a719d61be35fa")
#  

## ------------------------------------------------------------------------
# load BERT package
library(BERT)

# select length_weight data fields in release data required for input into the est_fish_weight function

data_store_release_RB_input <- data.frame(release_data_sim_RB$ASD_CODE,release_data_sim_RB$SEASON,
                                          release_data_sim_RB$CRUISE_ID,release_data_sim_RB$SET_ID,
                                          release_data_sim_RB$SPECIES_CODE,release_data_sim_RB$LENGTH_CM)

release_data_sim_RB$EST_WEIGHT_KG <- est_fish_weight(length_weight_data = length_weight_data_sim_RB,
                                                     length_data = data_store_release_RB_input)

# do the same for Reference Area tagged fish releases

data_store_release_RefArea_input <- data.frame(release_data_sim_RefArea$ASD_CODE,release_data_sim_RefArea$SEASON,
                                               release_data_sim_RefArea$CRUISE_ID,release_data_sim_RefArea$SET_ID,
                                               release_data_sim_RefArea$SPECIES_CODE,release_data_sim_RefArea$LENGTH_CM)

release_data_sim_RefArea$EST_WEIGHT_KG <- est_fish_weight(length_weight_data = length_weight_data_sim_RefArea,
                                                          length_data = data_store_release_RefArea_input)


## ----warning=FALSE,echo=TRUE,message=FALSE-------------------------------

# use RB as an index 
RB <- "RB_TOA"

# restrict Release data to relevant research block 
Release_data_RB <- release_data_sim_RB[release_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]

# restrict data to the relevant Research Block    
Catch_data_RB <- catch_data_sim_RB[catch_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]


## ------------------------------------------------------------------------
# note only 3 Research Blocks currently target TOP and all others target TOA
target_species <- "TOA"


## ------------------------------------------------------------------------

# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param release_data
Release_data_RB <- data.frame(Release_data_RB$SEASON,Release_data_RB$CRUISE_ID,
                              Release_data_RB$SET_ID,Release_data_RB$SPECIES_CODE,Release_data_RB$EST_WEIGHT_KG)

# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param catch_data
Catch_data_RB<- data.frame(Catch_data_RB$ID,Catch_data_RB$Season,Catch_data_RB$CRUISE_ID,
                           Catch_data_RB$SET_ID,Catch_data_RB$SPECIES_CODE,Catch_data_RB$CAUGHT_KG_TOTAL,
                           Catch_data_RB$LINE_LENGTH)


## ------------------------------------------------------------------------
Ref_area <- ifelse(RB=="TOP_RB","HIMI","RSR")


## ------------------------------------------------------------------------
Release_data_RefArea <- release_data_sim_RefArea[release_data_sim_RefArea[["REF_AREA_CODE"]]%in%Ref_area,]


Catch_data_RefArea <- catch_data_sim_RefArea[catch_data_sim_RefArea[["REF_AREA_CODE"]]%in%Ref_area,]


## ------------------------------------------------------------------------

# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param release_data
Release_data_RefArea <- data.frame(Release_data_RefArea$SEASON,Release_data_RefArea$CRUISE_ID,
                                   Release_data_RefArea$SET_ID,Release_data_RefArea$SPECIES_CODE,
                                   Release_data_RefArea$EST_WEIGHT_KG)

Catch_data_RefArea <- data.frame(Catch_data_RefArea$ID,Catch_data_RefArea$Season,Catch_data_RefArea$CRUISE_ID,
                                 Catch_data_RefArea$SET_ID,Catch_data_RefArea$SPECIES_CODE,
                                 Catch_data_RefArea$CAUGHT_KG_TOTAL,Catch_data_RefArea$LINE_LENGTH)


## ------------------------------------------------------------------------
# 2017 assessment estimates from WG-FSA-17
Ref_area_biomass <-ifelse(Ref_area%in%"HIMI",43993.03,92693.07)

Ref_area_CVs <-ifelse(Ref_area%in%"HIMI",0.072,0.073)



## ------------------------------------------------------------------------
Survey_est<- sort(unique(Catch_data_RB$Catch_data_RB.Season))


store_annual_estimates<-data.frame(matrix(0,nrow=length(Survey_est),ncol=11))
names(store_annual_estimates)=c("RB","Species","Season","Ref_area","RefArea_seabed_area",
                                "RefArea_N_Hauls","RB_seabedarea","RB_N_Hauls","Est","CI_lower","CI_upper")


store_annual_estimates$Season <- Survey_est

## ------------------------------------------------------------------------
for (y in Survey_est){
  
  # select the last three seasons in which fishing occurred 
  Seasons <- seq(min(Survey_est),y,1)
  
  if(length(Seasons)>3){Seasons <- Survey_est[(which(Survey_est%in%y)-2):which(Survey_est%in%y)]
  }else{Seasons <- Survey_est[1:which(Survey_est%in%y)]}
  
}

## ------------------------------------------------------------------------
RB_haul_data <- extract_catch_data_cpue_est(catch_data=Catch_data_RB,release_data=Release_data_RB,
                                            measure ="weights",target_species = target_species,
                                            catch_season = Seasons)

## ------------------------------------------------------------------------
head(RB_haul_data) 

## ------------------------------------------------------------------------
# get hauls in last 3 seasons in the Reference Areas that match the latest assessment period
# for HIMI 2017 hauls are excluded because the season and data were considered incomplete, 
# but for the simulation we assume complete data in 2017

Ref_area_seasons <-c(2015,2016,2017)

## ------------------------------------------------------------------------
RefArea_haul_data_test <- extract_catch_data_cpue_est(catch_data=Catch_data_RefArea,
                                                      release_data=Release_data_RefArea,
                                                      measure ="weights",
                                                      target_species = target_species,
                                                      catch_season = Ref_area_seasons)


## ----echo=FALSE----------------------------------------------------------
# just need a seabed area value so picked values for existing Research Blocks
if(RB=="RB_TOA"){
  RB_Seabed_area <- RB_seabed_area$Fishable_area[RB_seabed_area$Polys%in%"486_3"]
}
if(RB=="RB_TOP"){
  RB_Seabed_area <- RB_seabed_area$Fishable_area[RB_seabed_area$Polys%in%"5843a_1"]  
}

## ----eval=FALSE----------------------------------------------------------
#  # remove survey year col from the matrix for input into multi release function
#  RB_Seabed_area <- RB_seabed_area$Fishable_area[RB_seabed_area$Polys%in%RB]

## ------------------------------------------------------------------------
# change Reference area seabed area RSR_open
if(Ref_area=="RSR"){
  RefArea_Seabed_area <-Ref_area_seabed_area$Fishable_area[Ref_area_seabed_area$Polys%in%"RSR_open"]
}else{
  RefArea_Seabed_area <-Ref_area_seabed_area$Fishable_area[Ref_area_seabed_area$Polys%in%Ref_area]
}

## ------------------------------------------------------------------------
# number of bootstrap replicate samples
n_boot <- 10000

## ------------------------------------------------------------------------
obj <- CPUE_seabed(fish_CPUE_data=RB_haul_data,fish_area=RB_Seabed_area,
                   ref_CPUE_data=RefArea_haul_data_test,
                   ref_area=RefArea_Seabed_area,ref_bio=Ref_area_biomass,ref_bio_cv=Ref_area_CVs)

CPUE_seabed_boot <- cpue_bootstrap(obj,n_boot)

store_annual_estimates$RB[Survey_est%in%y] <- RB
store_annual_estimates$Species[Survey_est%in%y] <- as.character(target_species)
store_annual_estimates$Ref_area[Survey_est%in%y] <- Ref_area
store_annual_estimates$RefArea_seabed_area[Survey_est%in%y] <- RefArea_Seabed_area
store_annual_estimates$RefArea_biomass[Survey_est%in%y] <- Ref_area_biomass
store_annual_estimates$RefArea_N_Hauls[Survey_est%in%y] <-length(RefArea_haul_data_test)
store_annual_estimates$RefArea_CPUE[Survey_est%in%y] <- as.numeric(obj$data["ref_CPUE_est"])
store_annual_estimates$RB_seabedarea[Survey_est%in%y] <- RB_Seabed_area
store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <- length(RB_haul_data)
store_annual_estimates$RB_CPUE[Survey_est%in%y] <- as.numeric(obj$data["fish_CPUE_est"])
store_annual_estimates$Est[Survey_est%in%y] <- summary(CPUE_seabed_boot)["Est"]
store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(CPUE_seabed_boot)["2.5%"]
store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(CPUE_seabed_boot)["97.5%"]


store_biomass_estimates_CPUE <- subset(store_annual_estimates,select= c(RB,Species,Season,RB_N_Hauls,Est,CI_lower,CI_upper))
store_biomass_estimates_CPUE$Method<-rep("CPUE-by-seabed area",nrow(store_biomass_estimates_CPUE))


## ----echo=FALSE----------------------------------------------------------

# do the same for RB_TOP except dont need to go through all the steps 

# use RB as an index 
RB <- "RB_TOP"

# restrict Release data to relevant research block 
Release_data_RB <- release_data_sim_RB[release_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]

# restrict data to the relevant Research Block    
Catch_data_RB <- catch_data_sim_RB[catch_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]


# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param release_data
Release_data_RB <- data.frame(Release_data_RB$SEASON,Release_data_RB$CRUISE_ID,
                              Release_data_RB$SET_ID,Release_data_RB$SPECIES_CODE,Release_data_RB$EST_WEIGHT_KG)

# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param catch_data
Catch_data_RB<- data.frame(Catch_data_RB$ID,Catch_data_RB$Season,Catch_data_RB$CRUISE_ID,
                           Catch_data_RB$SET_ID,Catch_data_RB$SPECIES_CODE,Catch_data_RB$CAUGHT_KG_TOTAL,
                           Catch_data_RB$LINE_LENGTH)


## make sure you specifiy target species data 
target_species <- "TOP"

Ref_area <- ifelse(RB=="RB_TOP","HIMI","RSR")

Release_data_RefArea <- release_data_sim_RefArea[release_data_sim_RefArea[["REF_AREA_CODE"]]%in%Ref_area,]


Catch_data_RefArea <- catch_data_sim_RefArea[catch_data_sim_RefArea[["REF_AREA_CODE"]]%in%Ref_area,]


# select data for input into biomass estimate function see ?extract_catch_data_cpue_est param release_data
Release_data_RefArea <- data.frame(Release_data_RefArea$SEASON,Release_data_RefArea$CRUISE_ID,
                                   Release_data_RefArea$SET_ID,Release_data_RefArea$SPECIES_CODE,
                                   Release_data_RefArea$EST_WEIGHT_KG)

Catch_data_RefArea <- data.frame(Catch_data_RefArea$ID,Catch_data_RefArea$Season,Catch_data_RefArea$CRUISE_ID,
                                 Catch_data_RefArea$SET_ID,Catch_data_RefArea$SPECIES_CODE,
                                 Catch_data_RefArea$CAUGHT_KG_TOTAL,Catch_data_RefArea$LINE_LENGTH)


# 2017 values from WG-FSA-17
Ref_area_biomass <-ifelse(Ref_area%in%"HIMI",43993.03,92693.07)

Ref_area_CVs <-ifelse(Ref_area%in%"HIMI",0.072,0.073)


Survey_est<- sort(unique(Catch_data_RB$Catch_data_RB.Season))


store_annual_estimates<-data.frame(matrix(0,nrow=length(Survey_est),ncol=11))
names(store_annual_estimates)=c("RB","Species","Season","Ref_area","RefArea_seabed_area",
                                "RefArea_N_Hauls","RB_seabedarea","RB_N_Hauls","Est","CI_lower","CI_upper")


store_annual_estimates$Season <- Survey_est


for (y in Survey_est){
  
  # select the last three seasons in which fishing occurred 
  Seasons <- seq(min(Survey_est),y,1)
  
  if(length(Seasons)>3){Seasons <- Survey_est[(which(Survey_est%in%y)-2):which(Survey_est%in%y)]
  }else{Seasons <- Survey_est[1:which(Survey_est%in%y)]}
  
  
  RB_haul_data <- extract_catch_data_cpue_est(catch_data=Catch_data_RB,release_data=Release_data_RB,
                                              measure ="weights",target_species = target_species,
                                              catch_season = Seasons)
  
  # get hauls in last 3 seasons in the Reference Areas that match the latest assessment period
  # for HIMI 2017 hauls are excluded because the season and data were considered incomplete, 
  # but for the simulation we assume complete data in 2017
  
  Ref_area_seasons <-c(2015,2016,2017)
  
  
  RefArea_haul_data_test <- extract_catch_data_cpue_est(catch_data=Catch_data_RefArea,
                                                        release_data=Release_data_RefArea,
                                                        measure ="weights",
                                                        target_species =target_species,
                                                        catch_season = Ref_area_seasons)
  
  if(RB=="RB_TOA"){
    RB_Seabed_area <- RB_seabed_area$Fishable_area[RB_seabed_area$Polys%in%"486_3"]
  }
  if(RB=="RB_TOP"){
    RB_Seabed_area <- RB_seabed_area$Fishable_area[RB_seabed_area$Polys%in%"5843a_1"]  
  }
  
  
  # change Reference area seabed area RSR_open
  if(Ref_area=="RSR"){
    RefArea_Seabed_area <-Ref_area_seabed_area$Fishable_area[Ref_area_seabed_area$Polys%in%"RSR_open"]
  }else{
    RefArea_Seabed_area <-Ref_area_seabed_area$Fishable_area[Ref_area_seabed_area$Polys%in%Ref_area]
  }
  
  obj <- CPUE_seabed(fish_CPUE_data=RB_haul_data,fish_area=RB_Seabed_area,
                     ref_CPUE_data=RefArea_haul_data_test,
                     ref_area=RefArea_Seabed_area,ref_bio=Ref_area_biomass,ref_bio_cv=Ref_area_CVs)
  
  CPUE_seabed_boot <- cpue_bootstrap(obj,n_boot)
  
  store_annual_estimates$RB[Survey_est%in%y] <- RB
  store_annual_estimates$Species[Survey_est%in%y] <- as.character(target_species)
  store_annual_estimates$Ref_area[Survey_est%in%y] <- Ref_area
  store_annual_estimates$RefArea_seabed_area[Survey_est%in%y] <- RefArea_Seabed_area
  store_annual_estimates$RefArea_biomass[Survey_est%in%y] <- Ref_area_biomass
  store_annual_estimates$RefArea_N_Hauls[Survey_est%in%y] <-length(RefArea_haul_data_test)
  store_annual_estimates$RefArea_CPUE[Survey_est%in%y] <- as.numeric(obj$data["ref_CPUE_est"])
  store_annual_estimates$RB_seabedarea[Survey_est%in%y] <- RB_Seabed_area
  store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <- length(RB_haul_data)
  store_annual_estimates$RB_CPUE[Survey_est%in%y] <- as.numeric(obj$data["fish_CPUE_est"])
  store_annual_estimates$Est[Survey_est%in%y] <- summary(CPUE_seabed_boot)["Est"]
  store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(CPUE_seabed_boot)["2.5%"]
  store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(CPUE_seabed_boot)["97.5%"]
  
}


store_biomass_estimates_CPUE_RB_TOP <- subset(store_annual_estimates,select= c(RB,Species,Season,RB_N_Hauls,Est,CI_lower,CI_upper))
store_biomass_estimates_CPUE_RB_TOP$Method<-rep("CPUE-by-seabed area",nrow(store_biomass_estimates_CPUE_RB_TOP))

store_biomass_estimates_CPUE <- rbind(store_biomass_estimates_CPUE,store_biomass_estimates_CPUE_RB_TOP)


## ----echo=TRUE-----------------------------------------------------------
# number of bootstrap replicate samples
n_boot <- 10000

## ------------------------------------------------------------------------
### Estimate local biomass using the Chapman mark-recapture method
# remove within season recaptures 
recapture_data_sim_RB <- recapture_data_sim_RB[recapture_data_sim_RB$SEASON_RECAPTURE!=recapture_data_sim_RB$SEASON_RELEASE,]

# recaptures must be from the same research block of release
recapture_data_sim_RB <- recapture_data_sim_RB[recapture_data_sim_RB$RESEARCH_BLOCK_CODE_RECAPTURE%in%
                                                 recapture_data_sim_RB$RESEARCH_BLOCK_CODE_RELEASE,]

recapture_data_sim_RB <- droplevels(recapture_data_sim_RB[!is.na(recapture_data_sim_RB$RESEARCH_BLOCK_CODE_RECAPTURE),])


## ------------------------------------------------------------------------
# define Research blocks 
RB <- "RB_TOP"


Recapture_data <- recapture_data_sim_RB[recapture_data_sim_RB[["RESEARCH_BLOCK_CODE_RELEASE"]]%in%RB & recapture_data_sim_RB[["RESEARCH_BLOCK_CODE_RECAPTURE"]]%in%RB,]

Release_data <- release_data_sim_RB[release_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]

Catch_data <- catch_data_sim_RB[catch_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]

## ------------------------------------------------------------------------
Catch_data <- data.frame(Catch_data$ID,Catch_data$Season,Catch_data$CRUISE_ID,Catch_data$SET_ID,
                         Catch_data$SPECIES_CODE,Catch_data$CAUGHT_KG_TOTAL)
names(Catch_data) <- c("ID","Season","CRUISE_ID","SET_ID","SPECIES_CODE","CAUGHT_KG_TOTAL")
Release_data <- data.frame(Release_data$SEASON,Release_data$CRUISE_ID,Release_data$SET_ID,
                           Release_data$SPECIES_CODE,Release_data$EST_WEIGHT_KG)
names(Release_data) <- c("SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","EST_WEIGHT_KG")
Recapture_data <- data.frame(Recapture_data$SEASON_RELEASE,Recapture_data$SEASON_RECAPTURE,
                             Recapture_data$CRUISE_ID_RECAPTURE,Recapture_data$SET_ID_RECAPTURE,
                             Recapture_data$SPECIES_CODE_RECAPTURE)
names(Recapture_data)<- c("SEASON_RELEASE","SEASON_RECAPTURE","CRUISE_ID_RECAPTURE","SET_ID_RECAPTURE","SPECIES_CODE_RECAPTURE")

## ------------------------------------------------------------------------

# ensure only data from the relevant RB is included 
target_species<-"TOP"


## ------------------------------------------------------------------------
if(RB%in%c("486_2","RB_TOA")){
  # for 486_2 and RB_TOA WG-FSA-17 agreed tagged fish should only be 1 yr at liberty 
  # so only include recaptures from the previous year of release
  Recapture_data <- Recapture_data[Recapture_data$SEASON_RECAPTURE-Recapture_data$SEASON_RELEASE==1,]
}else{ # all other RBs recaptures are limited within 3 years of release
  Recapture_data <- Recapture_data[Recapture_data$SEASON_RECAPTURE-Recapture_data$SEASON_RELEASE<=3,]}


## ------------------------------------------------------------------------
Survey_years <- seq(min(Release_data[["SEASON"]],na.rm = TRUE),max(Release_data[["SEASON"]],na.rm = TRUE),1)


## ------------------------------------------------------------------------
# for data that is input into tag release and recap matrix  remove non-target species
Releases_input <-Release_data$SEASON[Release_data$SPECIES_CODE%in%target_species]

Recaps_input <- cbind(Recapture_data$SEASON_RELEASE[Recapture_data$SPECIES_CODE_RECAPTURE%in%target_species],Recapture_data$SEASON_RECAPTURE[Recapture_data$SPECIES_CODE_RECAPTURE%in%target_species])


tag_data_matrix <- extract_recaptures_season(release_data = Releases_input,recapture_data=Recaps_input,                              release_seasons = Survey_years)

## ------------------------------------------------------------------------

tag_data_matrix


## ------------------------------------------------------------------------
# No estimates in years where there are zero recaptures 
Survey_est<- unique(Recapture_data$SEASON_RECAPTURE)

## ------------------------------------------------------------------------
store_annual_estimates<-data.frame(matrix(0,nrow=length(Survey_est),ncol=10))
names(store_annual_estimates)=c("Species","RB","Season","N_recaptures","Total_Catch","RB_N_Hauls","Avail_tags", "Est","CI_lower","CI_upper") 

## make sure catch data extracts only include target species data
store_annual_estimates$Species <- rep(target_species,length(Survey_est))
store_annual_estimates$RB <- rep(RB,length(Survey_est))
store_annual_estimates$Season <- Survey_est


## ------------------------------------------------------------------------
for (y in Survey_est){
  
  # if Season releases is > 3 then restrict it to the past three years
  Season_releases <- seq(min(Survey_years),y,1)
  if(RB%in%c("486_2","RB_TOA") & length(Season_releases)>1){
    # 1 years of releases are included as the 1 year prior to the current season (y) 
    # are used in calculating the tagged fish available for recapture
    Season_releases <- seq(y-1,y,1)}else{
      if(length(Season_releases)>3){
        # 4 years of releases are included as the 3 years prior to the current season (y) 
        # are used in calculating the tagged fish available for recapture
        Season_releases <- seq(y-3,y,1)
      }
    }
  
}

## ------------------------------------------------------------------------
haul_data <- extract_catch_data_tag_est(catch_data=Catch_data,release_data=Release_data,
                                        recapture_data=Recapture_data,measure="weights",
                                        target_species=target_species,release_seasons =Season_releases,
                                        catch_season = y)


## ------------------------------------------------------------------------

head(haul_data)


## ------------------------------------------------------------------------
colSums(haul_data)


## ------------------------------------------------------------------------
tags <- tag_data_matrix[match(min(Season_releases),tag_data_matrix$Year):(match(max(Season_releases),tag_data_matrix$Year)-1),]
tags <- tags[,names(tags)%in%c("Releases",Season_releases)]

## ------------------------------------------------------------------------
## expanded tag_parameters
if(target_species%in%"TOP"){
  tag_pars <- list("mean_wt"=0, "method"="Chapman", "unit"="kg", "type"=1,
                   "tag_mort"=rep(0.1, length(Season_releases)), "reporting"=rep(1,length(Season_releases)), 
                   "nat_mort"=rep(0.155,length(Season_releases)), "chronic_shed"=rep(0.0084,length(Season_releases)),
                   "chronic_mort"=rep(0,length(Season_releases)))}else{
                     tag_pars <- list("mean_wt"=0, "method"="Chapman", "unit"="kg", "type"=1,
                                      "tag_mort"=rep(0.1, length(Season_releases)),
                                      "reporting"=rep(1,length(Season_releases)), 
                                      "nat_mort"=rep(0.13,length(Season_releases)),
                                      "chronic_shed"=rep(0.0084,length(Season_releases)),
                                      "chronic_mort"=rep(0,length(Season_releases)))}


## ------------------------------------------------------------------------

if(length(Season_releases)>2){
  
  ## now run the model
  obj <- multi_release(tags, hauls=haul_data, pars=tag_pars)
  test_boot <- tag_bootstrap(obj,n_boot,boot_zeroes=TRUE)
  
  store_annual_estimates$N_recaptures[Survey_est%in%y] <- sum(tags[,ncol(tags)])
  store_annual_estimates$Avail_tags[Survey_est%in%y] <- sum(obj$Avail_tags[,ncol(obj$Avail_tags)])
  store_annual_estimates$Total_Catch[Survey_est%in%y] <- sum(haul_data[,1])/1e3
  store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <-nrow(haul_data)   
  store_annual_estimates$Est[Survey_est%in%y] <- summary(test_boot)["Combined"]/1e3
  store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(test_boot)["2.5%"]/1e3  
  store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(test_boot)["97.5%"]/1e3 
  
}else{
  obj <- single_release(tags=as.numeric(tags[1]), catch=haul_data[,1], recaps=haul_data[,ncol(haul_data)-1],
                        method=tag_pars[["method"]],unit=tag_pars[["unit"]],
                        type=tag_pars[["type"]],tag_mort = tag_pars[["tag_mort"]][1],
                        reporting = tag_pars[["reporting"]][1],nat_mort = tag_pars[["nat_mort"]][1],
                        chronic_shed = tag_pars[["chronic_shed"]][1],chronic_mort = tag_pars[["chronic_mort"]][1])
  
  test_boot <- tag_bootstrap(obj,n_boot,boot_zeroes=TRUE)
  
  store_annual_estimates$N_recaptures[Survey_est%in%y] <- sum(haul_data[,ncol(haul_data)-1])
  store_annual_estimates$Avail_tags[Survey_est%in%y] <- obj$TagsAvailable
  store_annual_estimates$Total_Catch[Survey_est%in%y] <- sum(haul_data[,1])/1e3
  store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <- nrow(haul_data)
  store_annual_estimates$Est[Survey_est%in%y] <- summary(test_boot)[["N_hat"]]/1e3
  store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(test_boot)[["2.5%"]]/1e3
  store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(test_boot)[["97.5%"]]/1e3
  
}


# store Chapman estimates with inputs

store_biomass_estimates_chapman <-subset(store_annual_estimates,select=c(RB,Species,Season,Est,CI_lower,
                                                                         CI_upper,N_recaptures,RB_N_Hauls))
store_biomass_estimates_chapman$Method <- rep("Chapman",nrow(store_biomass_estimates_chapman))



## ----echo=FALSE----------------------------------------------------------
# Repeat Chapman estimate for RB_TOA except dont need to go though all the steps

# define Research blocks 
RB <- "RB_TOA"

Recapture_data <- recapture_data_sim_RB[recapture_data_sim_RB[["RESEARCH_BLOCK_CODE_RELEASE"]]%in%RB & recapture_data_sim_RB[["RESEARCH_BLOCK_CODE_RECAPTURE"]]%in%RB,]

Release_data <- release_data_sim_RB[release_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]

Catch_data <- catch_data_sim_RB[catch_data_sim_RB[["RESEARCH_BLOCK_CODE"]]%in%RB,]


Catch_data <- data.frame(Catch_data$ID,Catch_data$Season,Catch_data$CRUISE_ID,Catch_data$SET_ID,
                         Catch_data$SPECIES_CODE,Catch_data$CAUGHT_KG_TOTAL)
names(Catch_data) <- c("ID","Season","CRUISE_ID","SET_ID","SPECIES_CODE","CAUGHT_KG_TOTAL")
Release_data <- data.frame(Release_data$SEASON,Release_data$CRUISE_ID,Release_data$SET_ID,
                           Release_data$SPECIES_CODE,Release_data$EST_WEIGHT_KG)
names(Release_data) <- c("SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","EST_WEIGHT_KG")
Recapture_data <- data.frame(Recapture_data$SEASON_RELEASE,Recapture_data$SEASON_RECAPTURE,
                             Recapture_data$CRUISE_ID_RECAPTURE,Recapture_data$SET_ID_RECAPTURE,
                             Recapture_data$SPECIES_CODE_RECAPTURE)
names(Recapture_data)<- c("SEASON_RELEASE","SEASON_RECAPTURE","CRUISE_ID_RECAPTURE","SET_ID_RECAPTURE","SPECIES_CODE_RECAPTURE")


# ensure only data from the relevant RB is included 
target_species<-ifelse(RB=="RB_TOP","TOP","TOA")


if(RB=="RB_TOA"){
  # for 486_2 and RB_TOA WG-FSA-17 agreed tagged fish should only be 1 yr at liberty 
  # so only include recaptures from the previous year of release
  Recapture_data <- Recapture_data[Recapture_data$SEASON_RECAPTURE-Recapture_data$SEASON_RELEASE==1,]
}else{ # all other RBs recaptures are limited within 3 years of release
  Recapture_data <- Recapture_data[Recapture_data$SEASON_RECAPTURE-Recapture_data$SEASON_RELEASE<=3,]}


Survey_years <- seq(min(Release_data[["SEASON"]],na.rm = TRUE),max(Release_data[["SEASON"]],na.rm = TRUE),1)


# for data that is input into tag release and recap matrix  remove non-target species
Releases_input <-Release_data$SEASON[Release_data$SPECIES_CODE%in%target_species]

Recaps_input <- cbind(Recapture_data$SEASON_RELEASE[Recapture_data$SPECIES_CODE_RECAPTURE%in%target_species],Recapture_data$SEASON_RECAPTURE[Recapture_data$SPECIES_CODE_RECAPTURE%in%target_species])


tag_data_matrix <- extract_recaptures_season(release_data = Releases_input,recapture_data=Recaps_input,                              release_seasons = Survey_years)



# No estimates in years where there are zero recaptures 
Survey_est<- unique(Recapture_data$SEASON_RECAPTURE)

store_annual_estimates<-data.frame(matrix(0,nrow=length(Survey_est),ncol=10))
names(store_annual_estimates)=c("Species","RB","Season","N_recaptures","Total_Catch","RB_N_Hauls","Avail_tags", "Est","CI_lower","CI_upper") 

## make sure catch data extracts only include target species data
store_annual_estimates$Species <- rep(target_species,length(Survey_est))
store_annual_estimates$RB <- rep(RB,length(Survey_est))
store_annual_estimates$Season <- Survey_est


for (y in Survey_est){
  
  # if Season releases is > 3 then restrict it to the past three years
  Season_releases <- seq(min(Survey_years),y,1)
  if(RB%in%c("486_2","RB_TOA") & length(Season_releases)>1){
    # 1 years of releases are included as the 1 year prior to the current season (y) 
    # are used in calculating the tagged fish available for recapture
    Season_releases <- seq(y-1,y,1)}else{
      if(length(Season_releases)>3){
        # 4 years of releases are included as the 3 years prior to the current season (y) 
        # are used in calculating the tagged fish available for recapture
        Season_releases <- seq(y-3,y,1)
      }
    }
  
}

haul_data <- extract_catch_data_tag_est(catch_data=Catch_data,release_data=Release_data,
                                        recapture_data=Recapture_data,measure="weights",
                                        target_species=target_species,release_seasons =Season_releases,
                                        catch_season = y)


tags <- tag_data_matrix[match(min(Season_releases),tag_data_matrix$Year):(match(max(Season_releases),
                                                                                tag_data_matrix$Year)-1),]
tags <- tags[,names(tags)%in%c("Releases",Season_releases)]


## expanded tag_parameters
if(target_species%in%"TOP"){
  tag_pars <- list("mean_wt"=0, "method"="Chapman", "unit"="kg", "type"=1,
                   "tag_mort"=rep(0.1, length(Season_releases)), "reporting"=rep(1,length(Season_releases)), 
                   "nat_mort"=rep(0.155,length(Season_releases)), "chronic_shed"=rep(0.0084,length(Season_releases)),
                   "chronic_mort"=rep(0,length(Season_releases)))}else{
                     tag_pars <- list("mean_wt"=0, "method"="Chapman", "unit"="kg", "type"=1,
                                      "tag_mort"=rep(0.1, length(Season_releases)),
                                      "reporting"=rep(1,length(Season_releases)), 
                                      "nat_mort"=rep(0.13,length(Season_releases)),
                                      "chronic_shed"=rep(0.0084,length(Season_releases)),
                                      "chronic_mort"=rep(0,length(Season_releases)))}



if(length(Season_releases)>2){
  
  ## now run the model
  obj <- multi_release(tags, hauls=haul_data, pars=tag_pars)
  test_boot <- tag_bootstrap(obj,n_boot,boot_zeroes=TRUE)
  
  store_annual_estimates$N_recaptures[Survey_est%in%y] <- sum(tags[,ncol(tags)])
  store_annual_estimates$Avail_tags[Survey_est%in%y] <- sum(obj$Avail_tags[,ncol(obj$Avail_tags)])
  store_annual_estimates$Total_Catch[Survey_est%in%y] <- sum(haul_data[,1])/1e3
  store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <-nrow(haul_data)   
  store_annual_estimates$Est[Survey_est%in%y] <- summary(test_boot)["Combined"]/1e3
  store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(test_boot)["2.5%"]/1e3  
  store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(test_boot)["97.5%"]/1e3 
  
}else{
  obj <- single_release(tags=as.numeric(tags[1]), catch=haul_data[,1], recaps=haul_data[,ncol(haul_data)-1],
                        method=tag_pars[["method"]],unit=tag_pars[["unit"]],
                        type=tag_pars[["type"]],tag_mort = tag_pars[["tag_mort"]][1],
                        reporting = tag_pars[["reporting"]][1],nat_mort = tag_pars[["nat_mort"]][1],
                        chronic_shed = tag_pars[["chronic_shed"]][1],chronic_mort = tag_pars[["chronic_mort"]][1])
  
  test_boot <- tag_bootstrap(obj,n_boot,boot_zeroes=TRUE)
  
  store_annual_estimates$N_recaptures[Survey_est%in%y] <- sum(haul_data[,ncol(haul_data)-1])
  store_annual_estimates$Avail_tags[Survey_est%in%y] <- obj$TagsAvailable
  store_annual_estimates$Total_Catch[Survey_est%in%y] <- sum(haul_data[,1])/1e3
  store_annual_estimates$RB_N_Hauls[Survey_est%in%y] <- nrow(haul_data)
  store_annual_estimates$Est[Survey_est%in%y] <- summary(test_boot)[["N_hat"]]/1e3
  store_annual_estimates$CI_lower[Survey_est%in%y] <- summary(test_boot)[["2.5%"]]/1e3
  store_annual_estimates$CI_upper[Survey_est%in%y] <- summary(test_boot)[["97.5%"]]/1e3
  
}


# store Chapman estimates with inputs

store_biomass_estimates_chapman_TOA <-subset(store_annual_estimates,select=c(RB,Species,Season,Est,CI_lower,
                                                                             CI_upper,N_recaptures,RB_N_Hauls))
store_biomass_estimates_chapman_TOA$Method <- rep("Chapman",nrow(store_biomass_estimates_chapman_TOA))

# bind together with RB_TOP estimates 
store_biomass_estimates_chapman <- rbind(store_biomass_estimates_chapman,store_biomass_estimates_chapman_TOA)


## ------------------------------------------------------------------------

store_B_est_all <- rbind(store_biomass_estimates_chapman,data.frame(store_biomass_estimates_CPUE,N_recaptures=rep(NA,nrow(store_biomass_estimates_CPUE))))



## ----echo=TRUE-----------------------------------------------------------

# remove hauls and catch limit 
store_B_est_all <- subset(store_B_est_all, select=-c(RB_N_Hauls,Season))
store_B_est_all  <- store_B_est_all[order(store_B_est_all$RB),]
row.names(store_B_est_all)<- NULL

store_B_est_all

## ------------------------------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)


## ----echo=TRUE,warning=FALSE,fig.height=4, fig.width=6-------------------

# Plot biomass estimate time series per species and research block

Plot_data=store_B_est_all
Plot_data=Plot_data[Plot_data$RB%in%unique(Plot_data$RB[Plot_data$Method%in%"Chapman"]),]
limits=ggplot2::aes(ymin=CI_lower,ymax =CI_upper)
dodge = ggplot2::position_dodge(width=0.5)
p=ggplot2::ggplot(Plot_data,aes(x=as.factor(Method),y=Est,color=Method,group=Method,label=N_recaptures)) +
  geom_point(position = position_dodge(width=0.5)) + 
  geom_text(aes(label=ifelse(is.na(N_recaptures),"",N_recaptures)),
            hjust=-0.5,vjust=-1,color="black",
            size=2) +geom_errorbar(aes(ymin=CI_lower,ymax =CI_upper), 
                                   position=position_dodge(width=0.5), width=0.25,color="black")+
  facet_wrap(~RB,scales="free_y")+ylab("Estimated Biomass(t)") + xlab("Method") +
  scale_x_discrete(expand = c(0.1,0.1)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position = "none")
print(p)

