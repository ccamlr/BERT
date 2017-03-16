#' Sum planimetric seabed area
#'
#' Sum planimetric seabed area
#' @param bathymetry is data in a raster file format upon whch the calculation
#' is based, this function is called in seabed_area
#' @export
sum_area <- function(bathymetry){
  ##* the bathymetry argument isn't used, should it be 'x' (below)?
  plan_area_cells <- raster::freq(bathymetry, useNA="no")
  # multiply the total number of cells by cell res and convert to km2
  sum_area <- (sum(plan_area_cells[,2])*raster::xres(bathymetry)*raster::yres(bathymetry))/1e6
  sum_area
}

# This function is quite slow - speak to others on how to speed it up

#' Calculate planimetric seabed area
#'
#' Calculate planimetric seabed area
#' @param bathymetry is data in a raster file format upon which the calculation
#' is based, it is assumed that all bathymetry values above zero have been removed
#' @param ROI is the Region of Interest, this is assumed to be in a
#'  SpatialPolygonDataframe (SPDF) format
#' @param ROI_name is the name of the vector in the ROI that contains the
#' relevant polygon names/labels
#' @param fishable_area is TRUE if you would like to calculate the planimetric
#' seabed area in the fishable depth range (i.e. 800-1600m)
#' @param save_fishable_seabed_plots is TRUE if you woud like to save plots of
#' the fishable seabed area within the ROI
#' @export
seabed_area <- function(bathymetry, ROI, ROI_name, fishable_area, save_fishable_seabed_plots){
  # check ROI_name is a character vector
  if(length(names(ROI)[names(ROI)%in%ROI_name])>1) stop("ROI names in spatial polygon dataframe are not unique")
  if(is.null(names(ROI)[names(ROI)%in%ROI_name])) stop("ROI name is not included in spatial polygon dataframe ")
  # check that all spatial data are in the same projection
  ##* proj4string is in both raster and sp, use :: to specify which
  if(raster::proj4string(bathymetry)!=raster::proj4string(ROI)) stop("projection of bathymetry data does not match that of the ROI")

  area_names <- ROI@data[,names(ROI)%in%ROI_name]

  if(fishable_area==TRUE){
    plan_area <- data.frame(matrix(nrow=length(area_names),ncol=3))
    names(plan_area)<-c("Area","Total Seabed area km2","Fishable Seabed area km2")
  }else{
    plan_area <- data.frame(matrix(nrow=length(area_names),ncol=2))
    names(plan_area)<-c("Area","Total Seabed area km2")
  }

  for (name in area_names) {
  # index Research block
    Poly <- ROI[area_names%in%name,]

    Bathy <- raster::crop(bathymetry, raster::extent(Poly))
    Total_area <- raster::mask(Bathy,Poly)

    if(fishable_area==TRUE){
      Poly_mask <- Total_area > -1800 & Total_area < -600
      Fishable_area <- raster::mask(Total_area, Poly_mask, maskvalue=0)
      # estimate total planimetric area
      plan_area[area_names%in%name,1:3]<- c(as.character(name),sum_area(Total_area),sum_area(Fishable_area))

      if(save_fishable_seabed_plots==TRUE){

        # this plot will save in current wd - potentially modify to establish a folder etc
        png(paste(name,"_bathymetry_600_1800m_area.png",sep=""))
        par(mar=c(2,1,2,1))
        plot(Fishable_area,col=terrain.colors(255),main=paste(name,"_bathymetry_600_1800m_area",sep=""),axes=FALSE,box=FALSE)
        plot(Poly,add=TRUE,xaxt="n",yaxt="n",axes=FALSE)
        dev.off()
        # could potentially add graticules to plot
        #   plot(grat, add = TRUE, lty = 3,lwd=1.5)
        #   text(labs, lab = parse(text= labs$lab), col= "black", cex = 1)
      }
    }else{
      plan_area[area_names%in%name,1:2]<- c(as.character(name),sum_area(Total_area))
    }
  }
  plan_area
}
