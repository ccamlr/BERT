## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("ccamlr/BERT",build_vignettes=TRUE)
#  

## ------------------------------------------------------------------------
# create four simulated datasets similar to what would be provided in data extracts for Research Blocks 

catch_data_sim_RB <- rbind(data.frame(ID=seq(1,200,1),Season=rep(2017,200),CRUISE_ID=rep(100,200),SET_ID=seq(1,200,1),
                                SPECIES_CODE=rep("TOA",200),CAUGHT_KG_TOTAL=rnorm(200,mean=1257.80,sd=300),
                                LINE_LENGTH=rnorm(200,mean=10000,sd=2000),RESEARCH_BLOCK_CODE=rep("486_3",200)),
                           data.frame(ID=seq(201,400,1),Season=rep(2017,200),CRUISE_ID=rep(200,200),SET_ID=seq(1,200,1),
                                SPECIES_CODE=rep("TOP",200),CAUGHT_KG_TOTAL=rnorm(200,mean=274.93,sd=50),
                                LINE_LENGTH=rnorm(200,mean=10000,sd=2000),RESEARCH_BLOCK_CODE=rep("5843a_1",200)))

## ------------------------------------------------------------------------
# release data 
release_data_sim_RB <- rbind(data.frame(SEASON=c(rep(2015,50),rep(2016,50),rep(2017,50)),
                              CRUISE_ID=c(rep(40,50),rep(90,50),rep(100,50)),
                              SET_ID=sample(seq(1,200,1),150,replace=FALSE),SPECIES_CODE=rep("TOA",150),
                              RESEARCH_BLOCK_CODE=rep("486_3",150),ASD_CODE=rep("486",150),
                              LENGTH_CM=rnorm(150,150,10)),
                             data.frame(SEASON=c(rep(2015,50),rep(2016,50),rep(2017,50)),
                              CRUISE_ID=c(rep(45,50),rep(95,50),rep(200,50)),
                              SET_ID=sample(seq(1,200,1),150,replace=FALSE),
                              SPECIES_CODE=rep("TOP",150),
                              RESEARCH_BLOCK_CODE=rep("5843a_1",150),ASD_CODE=rep("5843a",150),
                              LENGTH_CM=rnorm(150,80,10)))

## ------------------------------------------------------------------------
# recapture data 
recapture_data_sim_RB <- rbind(data.frame(SEASON_RELEASE=c(rep(2015,2),rep(2016,6)),SEASON_RECAPTURE=rep(2017,8),
                              CRUISE_ID_RECAPTURE=rep(100,8),SET_ID_RECAPTURE=sample(seq(1,200,1),8),
                              SPECIES_CODE_RECAPTURE=rep("TOA",8),RESEARCH_BLOCK_CODE_RECAPTURE=rep("486_3",8),
                              RESEARCH_BLOCK_CODE_RELEASE=rep("486_3",8)),
                              data.frame(SEASON_RELEASE=c(rep(2015,2),rep(2016,2)),
                              SEASON_RECAPTURE=rep(2017,4),CRUISE_ID_RECAPTURE=rep(200,4),
                              SET_ID_RECAPTURE=sample(seq(1,200,1),4),SPECIES_CODE_RECAPTURE=rep("TOP",4),
                              RESEARCH_BLOCK_CODE_RECAPTURE=rep("5843a_1",4),
                              RESEARCH_BLOCK_CODE_RELEASE=rep("5843a_1",4)))

## ------------------------------------------------------------------------
# length_weight data 
length_weight_data_sim_RB <- rbind(data.frame(ASD_CODE=rep("486",200),SPECIES_CODE=rep("TOA",200),
                            LENGTH_CM=rnorm(200,150,50),WEIGHT_KG=rnorm(200,40,10)),
                            data.frame(ASD_CODE=rep("5843a",200),SPECIES_CODE=rep("TOP",200),
                            LENGTH_CM=rnorm(200,80,10),WEIGHT_KG=rnorm(200,10,4)))
# add some random error into weight estimates so model fit to simulated data is not perfect
error <- sample(seq(0,2,1),200,replace=TRUE)
# simulate weight based on length-weight relationship
length_weight_data_sim_RB$WEIGHT_KG <- exp(-12+3*log(length_weight_data_sim_RB$LENGTH_CM))*1.01 + error

## ------------------------------------------------------------------------
# catch data
catch_data_sim_RefArea <- rbind(data.frame(ID=seq(1,400,1),Season=rep(2017,400),CRUISE_ID=rep(70,400),
                            SET_ID=seq(1,400,1),SPECIES_CODE=rep("TOA",400),
                            CAUGHT_KG_TOTAL=rnorm(400,mean=2000,sd=1000),
                            LINE_LENGTH=rnorm(200,mean=10000,sd=2000),REF_AREA_CODE=rep("RSR",400)),
                            data.frame(ID=seq(401,800,1),Season=rep(2017,400),CRUISE_ID=rep(80,400),
                            SET_ID=seq(1,400,1),SPECIES_CODE=rep("TOP",400),
                            CAUGHT_KG_TOTAL=rnorm(400,mean=1530,sd=800),
                            LINE_LENGTH=rnorm(200,mean=10000,sd=2000),REF_AREA_CODE=rep("HIMI",400)))

# release data 
release_data_sim_RefArea <- rbind(data.frame(SEASON=rep(2017,100),CRUISE_ID=rep(70,100),
                            SET_ID=sample(seq(1,400,1),100,replace=FALSE),SPECIES_CODE=rep("TOA",100),
                            REF_AREA_CODE=rep("RSR",100),ASD_CODE=rep("881",100),LENGTH_CM=rnorm(100,150,10)),
                            data.frame(SEASON=rep(2017,100),CRUISE_ID=rep(80,100),
                            SET_ID=sample(seq(1,400,1),100,replace=FALSE),SPECIES_CODE=rep("TOP",100),
                            REF_AREA_CODE=rep("HIMI",100),ASD_CODE=rep("5852",100),LENGTH_CM=rnorm(100,80,10)))


# length_weight data 
length_weight_data_sim_RefArea <- rbind(data.frame(ASD_CODE=rep("881",400),SPECIES_CODE=rep("TOA",400),
                              LENGTH_CM=rnorm(400,150,30),WEIGHT_KG=rnorm(400,40,10)),
                              data.frame(ASD_CODE=rep("5852",400),SPECIES_CODE=rep("TOP",400),
                              LENGTH_CM=rnorm(400,80,10),WEIGHT_KG=rnorm(400,10,4)))
# add some random error into weight estimates so model fit to simulated data is not perfect
error <- sample(seq(0,2,1),400,replace=TRUE)
# simulate weight based on length-weight relationship
length_weight_data_sim_RefArea$WEIGHT_KG <- exp(-12+3*log(length_weight_data_sim_RefArea$LENGTH_CM))*1.01 + error


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


## ------------------------------------------------------------------------
# number of bootstrap replicate samples
n_boot <- 10000

