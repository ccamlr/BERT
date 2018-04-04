library(CCAMLRGIS)
library(BERT)
library(rgdal)

# load bathymetry data
load(paste(getwd(),"/","data/bathy_data.rda",sep=""))

RS_blks <- CCAMLRGIS::load_RBs("GEOJSON")
Ref_areas <- CCAMLRGIS::load_RefAreas("RDATA")
Ref_areas <- CCAMLRGIS::Ref_areas[Ref_areas$name%in%c("HIMI","RSR")]

# calculate seabed area for Research blocks 
RB_seabed_area <- seabed_area(bathy_data,RS_blks,"GAR_Short_Label",TRUE)
# store areas when this is done as processing is quite time consuming 
# calculate seabed area for reference area
Ref_area_sebed_area <- seabed_area(bathy_data,Ref_areas,"name",TRUE)
# store areas when this is done as processing is quite time consuming
