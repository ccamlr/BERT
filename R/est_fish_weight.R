#' Estimate tagged fish weight from fish lengths  
#'
#' Tagged fish releases and recaptures are not weighed but thier lengths are measured. 
#' This function uses length and weight measurements from commercial data in a log-linear model to estimate the weights of tagged fish from length data.
#' A dataframe is returned with all the information input into length_data plus the estimated weights    
#'
#'@param length_weight_data is a four column dataframe with the CCAMLR ASD code,species code, length (cm) and weight (kg) 
#'@param length_data is a six column dataframe with the CCAMLR ASD code,fishing season,cruise id, set id, species code, length (cm) 
#'@export
#'

est_fish_weight <- function(length_weight_data,length_data){
  
  
  names(length_weight_data)<- c("ASD_CODE","SPECIES_CODE","LENGTH_CM","WEIGHT_KG")
  
  # define variable names
  names(length_data) <- c("ASD_CODE","SEASON","CRUISE_ID","SET_ID","SPECIES_CODE","LENGTH_CM")
  ### Estimate weight of released fish that will be added to total catch in each Subarea
  ASDs<-unique(length_weight_data$ASD_CODE)
  
  Species<-unique(length_weight_data$SPECIES_CODE)
  
  length_weight_parameters<-NULL
  
  for (Sp in Species){
    
    for (ASD in ASDs) {
      
      data_temp<-length_weight_data[length_weight_data$ASD_CODE==ASD & length_weight_data$SPECIES_CODE==Sp,]
      if(nrow(data_temp)>0){
        
        thisLWT<- stats::lm(log(WEIGHT_KG)~log(LENGTH_CM),data=data_temp)
        # get correction factor for back transformation
        syx <- summary(thisLWT)$sigma
        cf <- exp((syx^2)/2)
        lwtIntercept_1<-as.numeric(exp(thisLWT$coefficients["(Intercept)"]))
        lwtIntercept_2<-as.numeric(thisLWT$coefficients["(Intercept)"])
        lwtSlope<-as.numeric(thisLWT$coefficients["log(LENGTH_CM)"])
        lwtMSE<-mean((thisLWT$residuals)^2)
        length_weight_parameters<-rbind(length_weight_parameters,cbind(Sp,ASD,lwtIntercept_1,lwtIntercept_2,lwtSlope,lwtMSE,cf))}
    }
  }
  
  length_weight_parameters<-data.frame(length_weight_parameters)
  names(length_weight_parameters)<-c("SPECIES_CODE","ASD_CODE","INTERCEPT1","INTERCEPT2","SLOPE","MSE","CF")
  
  # apply length weight relationship parameter estimates to lengths in tagging data to estimate weight of fish released
  for (Sp in Species){
    
    for (ASD in ASDs) {
      
      index_releases<-length_data$SPECIES_CODE==Sp & length_data$ASD_CODE==ASD
      index_param<-length_weight_parameters$SPECIES_CODE==Sp & length_weight_parameters$ASD_CODE==ASD
      
      if(nrow(length_weight_parameters[index_param,])>0 & nrow(length_data[index_releases,])>0){
        a <- as.numeric(as.character(length_weight_parameters$INTERCEPT2[index_param]))
        b <- as.numeric(as.character(length_weight_parameters$SLOPE[index_param]))
        c <- as.numeric(as.character(length_weight_parameters$CF[index_param]))
        # apply correction factor
        length_data$EST_WEIGHT_KG[index_releases]<-exp(a+b*log(length_data$LENGTH_CM[index_releases]))*cf
      }
      
    }
  }
  length_data
}