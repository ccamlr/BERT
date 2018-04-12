library(CCAMLRGIS)
library(BERT)
library(rgdal)
library(raster)
# load bathymetry data - request GEBCO_2014_600_1800m.zip from CCAMLR Secretariat 
# This should include 
# (1) bathymetry data file GEBCO_2014_geotiff_projected_cubic_resample_500_500m_depths_600_1800m2016-04-27.tif  
# (2) metadata file GEBCO_2014_600_1800m_metadata.txt
bathy_data<-raster("/Volumes/CCAMLR_WD_1/Lucy/GEBCO/GEBCO2014_final/CCAMLRGIS/GEBCO_2014_600_1800m/GEBCO_2014_geotiff_projected_cubic_resample_500_500m_depths_600_1800m2016-04-27.tif")

RS_blks <- CCAMLRGIS::load_RBs("GEOJSON")
# select on short label codes from spatial data frame
RS_blks <- RS_blks[,names(RS_blks)%in%"GAR_Short_Label"]
Ref_areas <- CCAMLRGIS::load_RefAreas("RDATA")


# calculate seabed area for Research blocks 
RB_seabed_area <- CCAMLRGIS::seabed_area(bathy_data,RS_blks,fishable_area=TRUE)
save(RB_seabed_area,file="data/RB_seabed_area.rda")

# store areas when this is done as processing is quite time consuming 
# calculate seabed area for reference area
Ref_area_seabed_area <- CCAMLRGIS::seabed_area(bathy_data,Ref_areas,fishable_area=TRUE)
# store areas when this is done as processing is quite time consuming
save(Ref_area_seabed_area,file="data/Ref_area_seabed_area.rda")



