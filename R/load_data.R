#' Load CCAMLR data extracts
#'
#' Load CCAMLR data extracts
#' @param data_dir is the directory into which the CCAMLR data extracts were
#' unzipped/copied to by the user
#' @export
load_data <- function(data_dir){
  data_files <- list.files(data_dir)
  # not sure if I should pattern match or specify extract name with date etc....discuss with Keith
  data_store_catch <- read.csv(paste(data_dir,"/",data_files[grep("catch",data_files)],sep=""))
  data_store_release <- read.csv(paste(data_dir,"/",data_files[grep("releases",data_files)],sep=""))
  data_store_recapture <- read.csv(paste(data_dir,"/",data_files[grep("recaptures",data_files)],sep=""))
  data_store_length_weight <- read.csv(paste(data_dir,"/",data_files[grep("lengthweight",data_files)],sep=""))

  obj <- list("Catch" = data_store_catch,
              "Releases" = data_store_release,
              "Recaptures" = data_store_recapture,
              "Length_weight"=data_store_length_weight )
  ## return the object
  obj
}

#' #' Load GEBCO bathymetry data
#' #'
#' #' Load GEBCO bathymetry data
#' #' @param update is TRUE if you want to load the most current bathymetry data from the CCAMLRGIS website or FALSE if you want to use the data that is stored in the package
#' #' (gebco 2014 30-arc second grid) will be downloaded and stored
#' #' @export
#' load_bathy_data <- function(update=TRUE){
#' 
#'   if(update==TRUE){
#'   #  use the url that stores the gebco data on our online geoserver - currently in correspondance with GEBCO/IBCSO about this
#'   # this works but currently bathymetry data is gebco 2008 and is just a tiff file (so just colour values no bathymetry values and no geographic information)
#'   ccamlrgisgebco <- "https://gis.ccamlr.org/geoserver/gis/wms?request=GetMap&service=WMS&version=1.1.0&layers=gis:geb08south_clip40_102020&styles=&bbox=-5398150.787274787,-5398800.520136088,5398800.5201460235,5398150.787284723&width=512&height=512&srs=EPSG:102020&format=image%2Ftiff"
#'  
#'    # ccamlrgisurl <- "https://gis.ccamlr.org/geoserver/gis/wms?version=1.1.0&request=GetMap&layers=gis:geb14_projected_cubic_resample_500_500m_depths_600_1800&styles=&bbox=-5397733.44606832,-5397766.55393168,5397766.55393168,5397733.44606832&width=512&height=512&srs=EPSG:102020&format=Gtiff"
#'   # > download.file(ccamlrgisurl,destfile = "bathy_data.tif")
#'   # 
#'   
#'   # if we upload gebco 2014 data as a GeoTiff then it should be accessible as:
#'   # new_test="https://gis.ccamlr.org/geoserver/gis/wms?request=GetMap&service=WMS&version=1.1.0&layers=gis:geb14south_clip40_102020&styles=&bbox=-5398150.787274787,-5398800.520136088,5398800.5201460235,5398150.787284723&width=512&height=512&srs=EPSG:102020&format=Gtiff"
#'   # download raster data to current file directory using the utils package
#'   download.file(ccamlrgisgebco,"test_gebco2008data.tif")
#'   Bathy_data <- raster::raster("test_gebco2008data.tif")
#'   # return bathymetry data
#'   }else{
#'   # need to fix this - data will not load as an .rda file anymore  
#'   load(paste(getwd(),"/","data/bathy_data.rda",sep=""))
#'   }
#'   Bathy_data
#'   
#' }
