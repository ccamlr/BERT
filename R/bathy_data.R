#' bathy_data 
#'
#'Bathymetry data sourced from GEBCO and processed by the Secretariat on the 27/04/2016
#' 
#' @format Raster Layer
#' \decribe{
#' \item{Driver} {GTiff/GeoTIFF}
#' Files:GEBCO/GEBCO2014_final/CCAMLRGIS/GEBCO_2014_geotiff_projected_cubic_resample_500_500m_depths_600_1800m2016-04-27.tif
#' Size is 21591, 21591
#' Coordinate System is:
#'   PROJCS["unnamed",
#'          GEOGCS["WGS 84",
#'                 DATUM["WGS_1984",
#'                       SPHEROID["WGS 84",6378137,298.257223563,
#'                                AUTHORITY["EPSG","7030"]],
#'                       AUTHORITY["EPSG","6326"]],
#'                 PRIMEM["Greenwich",0],
#'                 UNIT["degree",0.0174532925199433],
#'                 AUTHORITY["EPSG","4326"]],
#'          PROJECTION["Lambert_Azimuthal_Equal_Area"],
#'          PARAMETER["latitude_of_center",-90],
#'          PARAMETER["longitude_of_center",0],
#'          PARAMETER["false_easting",0],
#'          PARAMETER["false_northing",0],
#'          UNIT["metre",1,
#'               AUTHORITY["EPSG","9001"]]]
#' Origin = (-5397733.446068320423365,5397733.446068320423365)
#' Pixel Size = (500.000000000000000,-500.000000000000000)
#' Metadata: AREA_OR_POINT=Area
#' Image Structure Metadata:COMPRESSION=LZW
#' INTERLEAVE=BAND
#' Corner Coordinates:
#   Upper Left  (-5397733.446, 5397733.446) ( 45d 0' 0.00"W, 16d27'39.06"S)
#                                             Lower Left  (-5397733.446,-5397766.554) (135d 0' 0.63"W, 16d27'38.11"S)
#                                             Upper Right ( 5397766.554, 5397733.446) ( 45d 0' 0.63"E, 16d27'38.11"S)
#  Lower Right ( 5397766.554,-5397766.554) (135d 0' 0.00"E, 16d27'37.16"S)
# 'Center      (  16.5539317, -16.5539317) (135d 0' 0.00"E, 89d59'59.25"S)
#                                          Band 1 Block=256x256 Type=Float64, ColorInterp=Gray
#                                          Min=-1800.000 Max=-600.000 
#                                          Minimum=-1800.000, Maximum=-600.000, Mean=nan, StdDev=nan
#                                          NoData Value=-1.69999999999999994e+308
#                                          Metadata:
#                                          STATISTICS_MAXIMUM=-600.00006103516
#                                          STATISTICS_MEAN=nan
#                                          STATISTICS_MINIMUM=-1799.9998779297
#                                          STATISTICS_STDDEV=nan
#                                          
#                                          Source Data File: GEBCO_2014_1D.nc
#                                          Source Data Version: GEBCO_2014 (20150318)
#                                          Source Data Download Location: http://www.gebco.net/data_and_products/gridded_bathymetry_data/
#                                          Source Data References: Weatherall, P., K. M. Marks, M. Jakobsson, T. Schmitt, S. Tani, J. E. Arndt, M. Rovere, D. Chayes, V. Ferrini, and R. Wigley (2015), A new digital bathymetric model of the world's oceans, Earth and Space Science, 2, 331–345, doi:10.1002/2015EA000107; South of 60 degrees South, the GEBCO_2014 Grid is based on version 1 of the International Bathymetric Chart of the Southern Ocean (IBCSO): Arndt, J.E., H. W. Schenke, M. Jakobsson, F. Nitsche, G. Buys, B. Goleby, M. Rebesco, F. Bohoyo, J.K. Hong, J. Black, R. Greku, G. Udintsev, F. Barrios, W. Reynoso-Peralta, T. Morishita, R. Wigley, The International Bathymetric Chart of the Southern Ocean (IBCSO) Version 1.0 - A new bathymetric compilation covering circum-Antarctic waters, Geophysical Research Letters, doi: 10.1002/grl.50413"
#                                          
#                                          Additional information:
#                                          
#                                          "GEBCO'S GRIDS SHALL NOT BE USED FOR NAVIGATION OR FOR ANY OTHER PURPOSE INVOLVING SAFETY AT SEA.
#                                          
#                                          While every effort has been made to ensure reliability within the limits of present knowledge, the accuracy and completeness of GEBCO's gridded data sets cannot be guaranteed. No responsibility can be accepted by those involved in their compilation or publication for any consequential loss or damage arising from their use.
#                                          
#                                          User should be aware that GEBCO's grids are deep ocean products (based on trackline data from many different sources of varying quality and coverage) and do not include detailed bathymetry for shallow shelf waters. Although GEBCO's grids are presented at 30 arc-second and one arc-minute intervals of latitude and longitude, this does not imply that knowledge is available on sea floor depth at this resolution. Users are advised to consult the accompanying data set documentation before using the data sets."
#                                          