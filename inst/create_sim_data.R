
# Catch data requires the following data fields: ID, Season, CRUISE_ID, SET_ID, SPECIES_CODE, CAUGHT_KG_TOTAL, LINE_LENGTH, RESEARCH_BLOCK_CODE
# 
# Simulated catch data for TOA in Research Block RB_TOA and TOP in Research Block RB_TOP included 200 sets per Research Block for the CCAMLR 2017 fishing season. Line lengths were simulated based on a mean of 10 km (or 1000m) and total catch per set was simulated based on means from historical catch from those Research Blocks in the past 3 fishing seasons. 

# create four simulated datasets similar to what would be provided in data extracts for Research Blocks 

catch_data_sim_RB <- rbind(data.frame(ID=seq(1,200,1),Season=rep(2017,200),
                                      CRUISE_ID=rep(100,200),
                                      SET_ID=seq(1,200,1),
                                      SPECIES_CODE=rep("TOA",200),
                                      CAUGHT_KG_TOTAL=rnorm(200,mean=1257.80,sd=300),
                                      LINE_LENGTH=rnorm(200,mean=10000,sd=2000),
                                      RESEARCH_BLOCK_CODE=rep("RB_TOA",200)),
                           data.frame(ID=seq(201,400,1),
                                      Season=rep(2017,200),
                                      CRUISE_ID=rep(200,200),SET_ID=seq(1,200,1),
                                      SPECIES_CODE=rep("TOP",200),
                                      CAUGHT_KG_TOTAL=rnorm(200,mean=274.93,sd=50),
                                      LINE_LENGTH=rnorm(200,mean=10000,sd=2000),
                                      RESEARCH_BLOCK_CODE=rep("RB_TOP",200)))




# Release data requires the following data fields: SEASON (of release), CRUISE_ID, SET_ID, SPECIES_CODE,RESEARCH_BLOCK_CODE (of release), ASD_CODE, LENGTH_CM (of the released fish)
# 
# Simulated tagged fish release data for TOA in Research Block RB_TOA and TOP in Research Block RB_TOP included 50 released individuals from each CCAMLR 2016 fishing season from 2015 to 2017. Fish lengths were simulated based on mean fish lengths from the tagged fish released in the relevant Research Blocks.

# release data 
release_data_sim_RB <- rbind(data.frame(SEASON=c(rep(2015,50),rep(2016,50),rep(2017,50)),
                                        CRUISE_ID=c(rep(40,50),rep(90,50),rep(100,50)),
                                        SET_ID=sample(seq(1,200,1),150,replace=FALSE),
                                        SPECIES_CODE=rep("TOA",150),
                                        RESEARCH_BLOCK_CODE=rep("RB_TOA",150),
                                        ASD_CODE=rep("TOA_ASD",150),
                                        LENGTH_CM=rnorm(150,150,10)),
                             data.frame(SEASON=c(rep(2015,50),rep(2016,50),rep(2017,50)),
                                        CRUISE_ID=c(rep(45,50),rep(95,50),rep(200,50)),
                                        SET_ID=sample(seq(1,200,1),150,replace=FALSE),
                                        SPECIES_CODE=rep("TOP",150),
                                        RESEARCH_BLOCK_CODE=rep("RB_TOP",150),
                                        ASD_CODE=rep("TOP_ASD",150),
                                        LENGTH_CM=rnorm(150,80,10)))




# Recapture data requires the following data fields: SEASON_RELEASE, SEASON_RECAPTURE, CRUISE_ID_RECAPTURE, SET_ID_RECAPTURE, SPECIES_CODE_RECAPTURE,RESEARCH_BLOCK_CODE_RECAPTURE, RESEARCH_BLOCK_CODE_RELEASE
# 
# Simulated tagged fish recapture data for TOA in Research Block RB_TOA included 8 tagged fish recaptures and 4 recaptured individuals from Research Block RB_TOP from the CCAMLR 2017 fishing season. Note that no within season recaptures were simulated. 


# recapture data 
recapture_data_sim_RB <- rbind(data.frame(SEASON_RELEASE=c(rep(2015,2),rep(2016,6)),
                                          SEASON_RECAPTURE=rep(2017,8),
                                          CRUISE_ID_RECAPTURE=rep(100,8),
                                          SET_ID_RECAPTURE=sample(seq(1,200,1),8),
                                          SPECIES_CODE_RECAPTURE=rep("TOA",8),
                                          RESEARCH_BLOCK_CODE_RECAPTURE=rep("RB_TOA",8),
                                          RESEARCH_BLOCK_CODE_RELEASE=rep("RB_TOA",8)),
                               data.frame(SEASON_RELEASE=c(rep(2015,2),rep(2016,2)),
                                          SEASON_RECAPTURE=rep(2017,4),
                                          CRUISE_ID_RECAPTURE=rep(200,4),
                                          SET_ID_RECAPTURE=sample(seq(1,200,1),4),
                                          SPECIES_CODE_RECAPTURE=rep("TOP",4),
                                          RESEARCH_BLOCK_CODE_RECAPTURE=rep("RB_TOP",4),
                                          RESEARCH_BLOCK_CODE_RELEASE=rep("RB_TOP",4)))




# Length-weight data requires the following data fields: ASD_CODE,SPECIES_CODE, LENGTH_CM and WEIGHT_KG
# 
# Simulated length and weight data TOA in Research Block RB_TOA and TOP in Research Block RB_TOP included 200 individuals per Research Block. Fish lengths were simulated based on mean fish lengths from observer data that had been collected in those Research Blocks. Weights were simulated using approximate parameter values obtained from modelled log-linear relationships between length and weight data collected from these two areas. 
# 



# length_weight data 
length_weight_data_sim_RB <- rbind(data.frame(ASD_CODE=rep("TOA_ASD",200),
                                              SPECIES_CODE=rep("TOA",200),
                                              LENGTH_CM=rnorm(200,150,50),
                                              WEIGHT_KG=rnorm(200,40,10)),
                                   data.frame(ASD_CODE=rep("TOP_ASD",200),
                                              SPECIES_CODE=rep("TOP",200),
                                              LENGTH_CM=rnorm(200,80,10),
                                              WEIGHT_KG=rnorm(200,10,4)))
# add some random error into weight estimates so model fit to simulated data is not perfect
error <- sample(seq(0,2,1),200,replace=TRUE)
# simulate weight based on length-weight relationship
length_weight_data_sim_RB$WEIGHT_KG <- exp(-12+3*log(length_weight_data_sim_RB$LENGTH_CM))*1.01 + error


# Three datasets were simulated to represent data from Reference Areas similar to what would be provided in CCAMLR data extracts. 
# 
# These datasets include catch data, release data and length-weight data. The same data fields were required for these datasets as were required for the Research Blocks except the RESEARCH_BLOCK_CODE field is replaced with REF_AREA_CODE field. For more details on Reference Areas see Working Group paper WG-FSA-17/42


# catch data
catch_data_sim_RefArea <- rbind(data.frame(ID=seq(1,400,1),Season=rep(2017,400),CRUISE_ID=rep(70,400),
                                           SET_ID=seq(1,400,1),SPECIES_CODE=rep("TOA",400),
                                           CAUGHT_KG_TOTAL=rnorm(400,mean=2000,sd=1000),
                                           LINE_LENGTH=rnorm(200,mean=10000,sd=2000),
                                           REF_AREA_CODE=rep("RSR",400)),
                                data.frame(ID=seq(401,800,1),
                                           Season=rep(2017,400),CRUISE_ID=rep(80,400),
                                           SET_ID=seq(1,400,1),SPECIES_CODE=rep("TOP",400),
                                           CAUGHT_KG_TOTAL=rnorm(400,mean=1530,sd=800),
                                           LINE_LENGTH=rnorm(200,mean=10000,sd=2000)
                                           ,REF_AREA_CODE=rep("HIMI",400)))


# release data 
release_data_sim_RefArea <- rbind(data.frame(SEASON=rep(2017,100),CRUISE_ID=rep(70,100),
                                             SET_ID=sample(seq(1,400,1),100,replace=FALSE),
                                             SPECIES_CODE=rep("TOA",100),
                                             REF_AREA_CODE=rep("RSR",100),
                                             ASD_CODE=rep("TOA_ASD",100),LENGTH_CM=rnorm(100,150,10)),
                                  data.frame(SEASON=rep(2017,100),CRUISE_ID=rep(80,100),
                                             SET_ID=sample(seq(1,400,1),100,replace=FALSE),
                                             SPECIES_CODE=rep("TOP",100),
                                             REF_AREA_CODE=rep("HIMI",100),
                                             ASD_CODE=rep("TOP_ASD",100),
                                             LENGTH_CM=rnorm(100,80,10)))


# length_weight data 
length_weight_data_sim_RefArea <- rbind(data.frame(ASD_CODE=rep("TOA_ASD",400),
                                                   SPECIES_CODE=rep("TOA",400),
                                                   LENGTH_CM=rnorm(400,150,30),
                                                   WEIGHT_KG=rnorm(400,40,10)),
                                        data.frame(ASD_CODE=rep("TOP_ASD",400),
                                                   SPECIES_CODE=rep("TOP",400),
                                                   LENGTH_CM=rnorm(400,80,10),
                                                   WEIGHT_KG=rnorm(400,10,4)))
# add some random error into weight estimates so model fit to simulated data is not perfect
error <- sample(seq(0,2,1),400,replace=TRUE)
# simulate weight based on length-weight relationship
length_weight_data_sim_RefArea$WEIGHT_KG <- exp(-12+3*log(length_weight_data_sim_RefArea$LENGTH_CM))*1.01 + error

save(catch_data_sim_RB,release_data_sim_RB,recapture_data_sim_RB,length_weight_data_sim_RB,catch_data_sim_RefArea,release_data_sim_RefArea,length_weight_data_sim_RefArea,file="data/Simulated_data.rda")
