#' Plot a semi-regular grid as a ggmap.
#'
#' @param DT data.table for a single scene to be plotted
#' @param basegrid data.table that contains the row, col, lon and lat of the grid
#' @param rowvar character of the row name in basegrid
#' @param colvar character of the column name in basegrid
#' @param longvar character of the longitude name in basegrid
#' @param latvar character of latitude name in basegrid
#' @param id character of the ID of each cell of the grid - it has to be present both in DT and basegrid
#' @param zvar character of the variable in DT the user wants to plot
#' @param zoom integer for the zoom of the background map
#' @param loncen numeric value for the longitude of the center of the background map 
#' @param latcen numeric value for the latitude of the center of the background map 
#' @param alpha.white.backg numeric value for the alpha of the background map 
#' @param aspratio numeric value for the aspect ratio of the background map 
#' 
#' @return a ggmap object.
#' @import data.table
#' @import ggmap
#' @importFrom raster raster 
#' @importFrom raster rasterToPoints 
#' @importFrom raster values
#' @importFrom grDevices topo.colors
#' @examples 
#' plotraster(aod20150211, basegrid, id = "aodid", zvar = "aod47")
#' @export
plotraster <- function(DT, basegrid, rowvar = "hdfrow", colvar = "hdfcol", 
                       longvar = "lon", latvar = "lat", id, zvar, 
                       zoom = 9, loncen = NA, latcen = NA, 
                       alpha.white.backg = 0.5, aspratio = 1.05) {
  dt <- merge(basegrid, DT, by = id, all.x = TRUE)
  if(sum(c(longvar, latvar) %in% names(DT)) == 2) {
    setnames(dt, c(paste0(longvar, ".x"), paste0(latvar, ".x")), c(longvar, latvar))
  }
  if(sum(c(rowvar, colvar) %in% names(DT)) == 2) {
    setnames(dt, c(paste0(rowvar, ".x"), paste0(colvar, ".x")), c(rowvar, colvar))
  }
  if(is.na(loncen) | is.na(latcen)) {
    dtrange <- data.table(x = c(min(dt[, longvar, with = F], na.rm = TRUE),
                                max(dt[, longvar, with = F], na.rm = TRUE)),
                          y = c(min(dt[, latvar, with = F], na.rm = TRUE),
                                max(dt[, latvar, with = F], na.rm = TRUE)))
    backgroundmap <- get_map(location = c(lon = dtrange[, min(x) + (abs(max(x) - min(x))/2)], 
                                          lat = dtrange[, min(y) + (abs(max(y) - min(y))/2)]), zoom = zoom)
  } else {
    backgroundmap <- get_map(location = c(lon = loncen, lat = latcen), zoom = zoom)
  }
  # let's oredr by row and col
  dt <- dt[order(get(rowvar), get(colvar))]
  # create the raster
  myraster <-  raster(ncol = dt[, max(get(colvar))], 
                      nrow = dt[, max(get(rowvar))], 
                      xmn = unlist(dt[get(rowvar) == max(get(rowvar)) & get(colvar) == min(get(colvar)), 
                                      longvar, with = FALSE]), 
                      xmx = unlist(dt[get(rowvar) == min(get(rowvar)) & get(colvar) == max(get(colvar)), 
                                      longvar, with = FALSE]), 
                      ymn = unlist(dt[get(rowvar) == max(get(rowvar)) & get(colvar) == min(get(colvar)), 
                                      latvar, with = FALSE]), 
                      ymx = unlist(dt[get(rowvar) == min(get(rowvar)) & get(colvar) == max(get(colvar)), 
                                      latvar, with = FALSE]))
  values(myraster) <- unlist(dt[, zvar, with = FALSE]) # am I inserting them in the right way?
  myrasterdt <- data.table(rasterToPoints(myraster))
  # let's plot the map
  if(dim(myrasterdt)[1] <= 4) {
    ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) +
      geom_point(aes(x, y, col = layer),
                  alpha = 0.5, shape = 15,
                  data = myrasterdt) +
      coord_cartesian() +
      coord_fixed(aspratio) +
      scale_colour_gradientn(zvar, colours = topo.colors(225)) +
      guides(fill = guide_colorbar(barheight = 15)) + 
      theme_bw(18) + theme(axis.line = element_blank(), 
                           axis.text = element_blank(), 
                           axis.ticks = element_blank(), 
                           axis.title = element_blank(),
                           panel.border = element_blank(), 
                           panel.grid = element_blank()) 
    
  } else {
    ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) +
      geom_raster(aes(x, y, fill = layer),
                  alpha = 0.5,
                  data = myrasterdt) +
      coord_cartesian() +
      coord_fixed(aspratio) +
      scale_fill_gradientn(zvar, colours = topo.colors(225)) +
      guides(fill = guide_colorbar(barheight = 15)) + 
      theme_bw(18) + theme(axis.line = element_blank(), 
                           axis.text = element_blank(), 
                           axis.ticks = element_blank(), 
                           axis.title = element_blank(),
                           panel.border = element_blank(), 
                           panel.grid = element_blank()) 
  }
}
