library(CCAMLRGIS)
library(BERT)
library(rgdal)

# load bathymetry data - request GEBCO_2014_600_1800m.zip from CCAMLR Secretariat 
# This should include bathymetry data file GEBCO_2014_geotiff_projected_cubic_resample_500_500m_depths_600_1800m2016-04-27.tif  
# metadata file GEBCO_2014_600_1800m_metadata.txt

RS_blks <- CCAMLRGIS::load_RBs("GEOJSON")
Ref_areas <- CCAMLRGIS::load_RefAreas("RDATA")
Ref_areas <- CCAMLRGIS::Ref_areas[Ref_areas$name%in%c("HIMI","RSR")]

# calculate seabed area for Research blocks 
RB_seabed_area <- CCAMLRGIS::seabed_area(bathy_data,RS_blks,fishable_area=TRUE)
save(RB_seabed_area,file="data/RB_seabed_area.rda")

# store areas when this is done as processing is quite time consuming 
# calculate seabed area for reference area
Ref_area_sebed_area <- CCAMLRGIS::seabed_area(bathy_data,Ref_areas,fishable_area=TRUE)
# store areas when this is done as processing is quite time consuming
save(Ref_area_seabed_area,file="data/Ref_area_sebed_area.rda")


