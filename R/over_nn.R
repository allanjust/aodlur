
# Simple 'spatial only' join between two point layers according to 'nearest neighbor' criterion
over_nn = function(jointo.pts, joinfrom.pts) {
  
  # Both objects need to be SpatialPoints or SpatialPointsDataFrame
  stopifnot(class(jointo.pts) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
  stopifnot(class(joinfrom.pts) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
  
  # Pairwise distance matrix
  dist_matrix = spDists(jointo.pts, joinfrom.pts)
  
  # ID of nearest point in 'joinfrom.pts' for each point in 'jointo.pts'
  nn_ids = apply(dist_matrix, 1, which.min)
  
  # Take attributes from 'joinfrom.pts' for the specified IDs
  joinfrom.pts@data[nn_ids, ]

}

# Example:

# library(aodlur)
# library(sp)
# 
# data(cities)
# data(county)
# 
# over_nn(jointo.pts = cities, joinfrom.pts = county)
# 
# cities@data = cbind(cities@data, over_nn(cities, county))
# cities@data

