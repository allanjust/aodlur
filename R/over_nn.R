
# Simple 'spatial only' join between two point layers according to 'nearest neighbor' criterion
over_nn = function(jointo.pts, joinfrom.pts) {
  
  # Both objects need to be SpatialPoints or SpatialPointsDataFrame
  stopifnot(class(jointo.pts) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
  stopifnot(class(joinfrom.pts) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
  
  # Objects need to be in the same CRS
  stopifnot(identicalCRS(jointo.pts, joinfrom.pts))
  
  # Pairwise distance matrix
  dist_matrix = spDists(jointo.pts, joinfrom.pts)
  
  # ID of nearest point in 'joinfrom.pts' for each point in 'jointo.pts'
  nn_ids = apply(dist_matrix, 1, which.min)
  
  # Take attributes from 'joinfrom.pts' for the specified IDs
  joinfrom.pts@data[nn_ids, ]

}

# Example:

# devtools::install_github("allanjust/aodlur")

# library(aodlur)
# library(sp)
# 
# Data from MODIS and Calipso satellites
# data(mod)
# data(cal)
# 
# Subset a single day
# cal_sub = cal[cal$date == "2006-07-15", ]
# mod_sub = mod[mod$date == "2006-07-15", ]
# head(cal_sub@data)
# 
# For each Calipso measurement, find nearest MODIS measurement
# cal_over_mod = 
#   over_nn(
#     jointo.pts = cal_sub, 
#     joinfrom.pts = mod_sub
#   )
# 
# Join matching MODIS measurements to the attribute table of Calipso
# cal_sub@data = cbind(cal_sub@data, cal_over_mod)
# head(cal_sub@data)

