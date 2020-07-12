# Code to get the samples that must be analayzed and create a shapefile

#------ ------ Parameters ---------------
# data.tb   - Samples to analyze
# name_file - give a name to shapefile 
#----------------------------------------

point_to_shape <- function (data.tb, name_file = "NULL")
{
  group_shape <-
    dplyr::select(data.tb,
                  longitude,
                  latitude,
                  start_date,
                  end_date,
                  label,
                  id_sample,
                  id_neuron)
  
  sp_data.tb.df <- as.data.frame(group_shape)
  
  points_SF <- as.data.frame(sp_data.tb.df)
  xy <- points_SF[, c(1, 2)]
  
  sp_data.df <- sp::SpatialPointsDataFrame(
    coords = xy,
    data = points_SF,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )
  
  rgdal::writeOGR(sp_data.df,
                  dsn = '.',
                  layer = name_file,
                  driver = "ESRI Shapefile")
  
  
}
