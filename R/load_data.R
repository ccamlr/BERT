#' Load CCAMLR data extracts
#'
#' Load CCAMLR data extracts
#' @param data_dir is the directory into which the CCAMLR data extracts were
#' unzipped/copied to by the user
#' @export
load_data <- function(data_dir){
  data_files <- list.files(data_dir,pattern = ".csv")
  # using pattern match but could specify exact names 
  # if data_files length == 4 then all data extracts are required, but if length is equal to 3 then recaps are not required
  if (length(data_files)==4){
  data_store_catch <- read.csv(paste(data_dir,"/",data_files[grep("catch",data_files)],sep=""))
  data_store_release <- read.csv(paste(data_dir,"/",data_files[grep("releases",data_files)],sep=""))
  data_store_recapture <- read.csv(paste(data_dir,"/",data_files[grep("recaptures",data_files)],sep=""))
  data_store_length_weight <- read.csv(paste(data_dir,"/",data_files[grep("lengthweight",data_files)],sep=""))

  obj <- list("Catch" = data_store_catch,
              "Releases" = data_store_release,
              "Recaptures" = data_store_recapture,
              "Length_weight"=data_store_length_weight )}else{
    data_store_catch <- read.csv(paste(data_dir,"/",data_files[grep("catch",data_files)],sep=""))
    data_store_release <- read.csv(paste(data_dir,"/",data_files[grep("releases",data_files)],sep=""))
    data_store_length_weight <- read.csv(paste(data_dir,"/",data_files[grep("lengthweight",data_files)],sep=""))
    
    obj <- list("Catch" = data_store_catch,
                "Releases" = data_store_release,
                "Length_weight"=data_store_length_weight )}
  ## return the object
  obj
}

