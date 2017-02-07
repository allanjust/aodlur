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
#' @param ggmap if TRUE (default) print the ggmap 
#' @param zoom integer for the zoom of the background map
#' @param loncen numeric value for the longitude of the center of the background map 
#' @param latcen numeric value for the latitude of the center of the background map 
#' @param alpha.layer numeric value for the alpha of the layer 
#' @param alpha.white.backg numeric value for the alpha of the background map 
#' @param aspratio numeric value for the aspect ratio of the background map 
#' 
#' @return a ggmap object.
#' @import data.table
#' @import ggmap
#' @import gdalUtils
#' @import viridis
#' @importFrom raster raster 
#' @importFrom raster rasterToPoints 
#' @importFrom raster values
#' @importFrom grDevices topo.colors
#' @examples 
#' plotraster(aod20150211, basegrid, id = "aodid", zvar = "aod47")
#' @export
plotraster <- function (DT, basegrid, rowvar = "hdfrow", colvar = "hdfcol", 
                        longvar = "lon", latvar = "lat", id, zvar, ggmap = TRUE, 
                        zoom = 9, loncen = NA, latcen = NA, alpha.layer = 0.5,
                        alpha.white.backg = 0.5, aspratio = 1.05) 
{
  dt <- merge(DT, basegrid[, c(rowvar, colvar, longvar, latvar, id), with = FALSE], 
              by = id, all.x = TRUE)
  if (sum(c(longvar, latvar) %in% names(DT)) == 2) {
    setnames(dt, c(paste0(longvar, ".y"), paste0(latvar, ".y")), c(longvar, latvar))
  }
  if (sum(c(rowvar, colvar) %in% names(DT)) == 2) {
    setnames(dt, c(paste0(rowvar, ".y"), paste0(colvar, ".y")), c(rowvar, colvar))
  }
  if (ggmap) {
    if (is.na(loncen) | is.na(latcen)) {
      dtrange <- data.table(x = c(min(dt[, longvar, with = F], na.rm = TRUE), 
                                  max(dt[, longvar, with = F], na.rm = TRUE)), 
                            y = c(min(dt[, latvar, with = F], na.rm = TRUE), 
                                  max(dt[, latvar, with = F], na.rm = TRUE)))
      backgroundmap <- get_map(location = c(lon = dtrange[, min(x) + (abs(max(x) - min(x))/2)], 
                                            lat = dtrange[, min(y) + (abs(max(y) - min(y))/2)]), 
                               zoom = zoom)
    }
    else {
      backgroundmap <- get_map(location = c(lon = loncen, lat = latcen), zoom = zoom)
    }
    rastergrid <- setDT(expand.grid(dt[, min(get(rowvar))]:dt[, max(get(rowvar))], 
                                    dt[, min(get(colvar))]:dt[, max(get(colvar))]))
    setnames(rastergrid, c(rowvar, colvar))
    dt <- merge(rastergrid, dt, c(rowvar, colvar), all.x = TRUE)
    dt <- dt[order(get(rowvar), get(colvar))]
    myraster <- raster(ncol = dt[, diff(range(get(colvar))) + 1], 
                       nrow = dt[, diff(range(get(rowvar))) + 1], 
                       xmn = dt[, min(get(longvar), na.rm = TRUE)], 
                       xmx = dt[, max(get(longvar), na.rm = TRUE)], 
                       ymn = dt[, min(get(latvar), na.rm = TRUE)], 
                       ymx = dt[, max(get(latvar), na.rm = TRUE)])
    values(myraster) <- unlist(dt[, zvar, with = FALSE])
    myrasterdt <- data.table(rasterToPoints(myraster))
    if (nrow(myrasterdt) <= 4) {
      ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) + 
        geom_point(aes(x, y, col = layer), alpha = alpha.layer, shape = 15, data = myrasterdt) + 
        coord_cartesian() + 
        coord_fixed(aspratio) + 
        scale_color_viridis(zvar, option = "magma") + 
        guides(color = guide_colorbar(barheight = 15)) + 
        theme_bw(18) + theme(axis.line = element_blank(), 
                             axis.text = element_blank(), axis.ticks = element_blank(), 
                             axis.title = element_blank(), panel.border = element_blank(), 
                             panel.grid = element_blank())
    }
    else {
      ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) + 
        geom_raster(aes(x, y, fill = layer), alpha = alpha.layer, data = myrasterdt) + 
        coord_cartesian() + 
        coord_fixed(aspratio) + 
        scale_fill_viridis(zvar, option = "magma") + 
        guides(fill = guide_colorbar(barheight = 15)) + 
        theme_bw(18) + theme(axis.line = element_blank(), 
                             axis.text = element_blank(), axis.ticks = element_blank(), 
                             axis.title = element_blank(), panel.border = element_blank(), 
                             panel.grid = element_blank())
    }
  }
  else {
    setnames(dt, c(rowvar, colvar, zvar), c("row", "col", "zvar"))
    ggplot() + geom_raster(aes(col, -row, fill = zvar), alpha = alpha.layer, data = dt) + 
      scale_fill_viridis(zvar, option = "magma") + 
      guides(fill = guide_colorbar(barheight = 15)) +  
      theme_bw(18) + theme(axis.line = element_blank(), 
                           axis.text = element_blank(), axis.ticks = element_blank(), 
                           axis.title = element_blank(), panel.border = element_blank(), 
                           panel.grid = element_blank(), panel.background = element_blank())
  }
}

# old function
# plotraster <- function(DT, basegrid, rowvar = "hdfrow", colvar = "hdfcol", 
#                        longvar = "lon", latvar = "lat", id, zvar, 
#                        ggmap = TRUE, zoom = 9, loncen = NA, latcen = NA, 
#                        alpha.white.backg = 0.5, aspratio = 1.05) {
#   dt <- merge(basegrid, DT, by = id, all.x = TRUE)
#   if(sum(c(longvar, latvar) %in% names(DT)) == 2) {
#     setnames(dt, c(paste0(longvar, ".x"), paste0(latvar, ".x")), c(longvar, latvar))
#   }
#   if(sum(c(rowvar, colvar) %in% names(DT)) == 2) {
#     setnames(dt, c(paste0(rowvar, ".x"), paste0(colvar, ".x")), c(rowvar, colvar))
#   }
#   # let's plot the map
#   if(ggmap) {
#     # let's oredr by row and col
#     dt <- dt[order(get(rowvar), get(colvar))]
#     # create the raster
#     myraster <-  raster(ncol = dt[, max(get(colvar))], 
#                         nrow = dt[, max(get(rowvar))], 
#                         xmn = unlist(dt[get(rowvar) == max(get(rowvar)) & get(colvar) == min(get(colvar)), 
#                                         longvar, with = FALSE]), 
#                         xmx = unlist(dt[get(rowvar) == min(get(rowvar)) & get(colvar) == max(get(colvar)), 
#                                         longvar, with = FALSE]), 
#                         ymn = unlist(dt[get(rowvar) == max(get(rowvar)) & get(colvar) == min(get(colvar)), 
#                                         latvar, with = FALSE]), 
#                         ymx = unlist(dt[get(rowvar) == min(get(rowvar)) & get(colvar) == max(get(colvar)), 
#                                         latvar, with = FALSE]))
#     values(myraster) <- unlist(dt[, zvar, with = FALSE]) # am I inserting them in the right way?
#     myrasterdt <- data.table(rasterToPoints(myraster))
#     if(is.na(loncen) | is.na(latcen)) {
#       dtrange <- data.table(x = c(min(dt[, longvar, with = F], na.rm = TRUE),
#                                   max(dt[, longvar, with = F], na.rm = TRUE)),
#                             y = c(min(dt[, latvar, with = F], na.rm = TRUE),
#                                   max(dt[, latvar, with = F], na.rm = TRUE)))
#       backgroundmap <- get_map(location = c(lon = dtrange[, min(x) + (abs(max(x) - min(x))/2)], 
#                                             lat = dtrange[, min(y) + (abs(max(y) - min(y))/2)]), zoom = zoom)
#     } else {
#       backgroundmap <- get_map(location = c(lon = loncen, lat = latcen), zoom = zoom)
#     }
#     if(dim(myrasterdt)[1] <= 4) {
#       ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) +
#         geom_point(aes(x, y, col = layer),
#                    alpha = 0.5, shape = 15,
#                    data = myrasterdt) +
#         coord_cartesian() +
#         coord_fixed(aspratio) +
#         scale_colour_gradientn(zvar, colours = topo.colors(225)) +
#         guides(fill = guide_colorbar(barheight = 15)) + 
#         theme_bw(18) + theme(axis.line = element_blank(), 
#                              axis.text = element_blank(), 
#                              axis.ticks = element_blank(), 
#                              axis.title = element_blank(),
#                              panel.border = element_blank(), 
#                              panel.grid = element_blank()) 
#       
#     } else {
#       ggmap(backgroundmap, extent = "normal", darken = c(alpha.white.backg, "white"), maprange = TRUE) +
#         geom_raster(aes(x, y, fill = layer),
#                     alpha = 0.5,
#                     data = myrasterdt) +
#         coord_cartesian() +
#         coord_fixed(aspratio) +
#         scale_fill_gradientn(zvar, colours = topo.colors(225)) +
#         guides(fill = guide_colorbar(barheight = 15)) + 
#         theme_bw(18) + theme(axis.line = element_blank(), 
#                              axis.text = element_blank(), 
#                              axis.ticks = element_blank(), 
#                              axis.title = element_blank(),
#                              panel.border = element_blank(), 
#                              panel.grid = element_blank()) 
#     }
#   } else {
#     setnames(dt, c(rowvar, colvar, zvar), c("row", "col", "zvar"))
#     ggplot() +
#       geom_raster(aes(col, -row, fill = zvar),
#                   alpha = 0.5,
#                   data = dt) +
#       scale_fill_gradientn(zvar, colours = topo.colors(225), na.value = "white") +
#       guides(fill = guide_colorbar(barheight = 15)) + 
#       theme_bw(18) + theme(axis.line = element_blank(), 
#                            axis.text = element_blank(), 
#                            axis.ticks = element_blank(), 
#                            axis.title = element_blank(),
#                            panel.border = element_blank(), 
#                            panel.grid = element_blank(),
#                            panel.background = element_blank()) 
#   }
# }
