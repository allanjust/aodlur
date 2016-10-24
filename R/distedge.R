#' Calculate raster distance to an edge.
#'
#' @param DT a data.table
#' @param rowvar character of the column name for row
#' @param colvar character of the column name for column
#' @param nonmissingmask character of the column name for the non missing mask
#' 
#' @return vector of distance to edge.
#' @import data.table
#' @importFrom raster raster 
#' @importFrom raster values 
#' @importFrom raster getValues 
#' @importFrom raster projection 
#' @importFrom raster distance
#' @examples  
#' aod20150211 <- merge(aod20150211, basegrid, by = "aodid", all.x = TRUE)
#' aod20150211[, revna := NA_integer_]
#' aod20150211[is.na(aod47), revna := 1]
#' distedge(aod20150211, rowvar = "hdfrow", colvar = "hdfcol", nonmissingmask = "revna")
#' @export
distedge <- function(DT, rowvar = "hdfrow", colvar = "hdfcol", nonmissingmask = "revna") {
  if (DT[, sum(is.na(get(nonmissingmask))) != 0]) {
    data <- DT[, c(rowvar, colvar, nonmissingmask), with = FALSE]
    # setorderv(DT, cols = c(rowvar, colvar)) 
    setorderv(data, cols = c(rowvar, colvar)) 
    # if we set the order it mean that the order of DT is going to change before computing distedgevector
    # create raster to hold this data  
    rasterna <-  raster(ncol = data[, diff(range(get(colvar))) + 1], 
                        nrow = data[, diff(range(get(rowvar))) + 1], 
                        xmn = data[get(rowvar) == max(get(rowvar)) & get(colvar) == min(get(colvar)), get(colvar)], 
                        xmx = data[get(rowvar) == min(get(rowvar)) & get(colvar) == max(get(colvar)), get(colvar)], 
                        ymn = data[get(rowvar) == min(get(rowvar)) & get(colvar) == min(get(colvar)), get(rowvar)], 
                        ymx = data[get(rowvar) == max(get(rowvar)) & get(colvar) == max(get(colvar)), get(rowvar)])
    values(rasterna) <- unlist(data[, nonmissingmask, with = F])
    # print(rasterna)
    # plot(rasterna)
    # computing distance requires a projection - we approximate
    projection(rasterna) = "+proj=tmerc +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +units=km"
    distna <- distance(rasterna)
    # class(distna)
    # plot(distna)
    # find regions with aod that near edges
    # plot(distna <= distedge & distna > 0)
    # return vector with distance to edge
    # distedgevector <- getValues(distna)
    data[, distedgevector := getValues(distna)]
    finalorder <- DT[, c(rowvar, colvar), with = FALSE]
    finalorder <- merge(finalorder, data, by = c(rowvar, colvar), sort = FALSE)
    distedgevector <- finalorder[, distedgevector]
    distedgevector
  } else rep(0, nrow(DT))
}

# aod20150211 <- merge(aod20150211, basegrid, by = "aodid", all.x = TRUE)
# aod20150211[, revna := NA_integer_]
# aod20150211[is.na(aod47), revna := 1]
# distedge(aod20150211, rowvar = "hdfrow", colvar = "hdfcol", nonmissingmask = "revna")
# # check the order
# aod2 <- aod20150211[order(aod47),]
# aod3 <- aod20150211[order(cloudmask),]
# aod2[, distedg := distedge(.SD, rowvar = "hdfrow", colvar = "hdfcol", nonmissingmask = "revna")]
# aod3[, distedg := distedge(.SD, rowvar = "hdfrow", colvar = "hdfcol", nonmissingmask = "revna")]
# aod2 <- aod2[order(hdfrow, hdfcol)]
# aod3 <- aod3[order(hdfrow, hdfcol)]
# identical(aod2$distedg, aod3$distedg)
# aod2 <- aod2[order(aod47),]
# aod3 <- aod3[order(cloudmask),]
# plotraster(aod20150211, basegrid, id = "aodid", zvar = "aod47")
# plotraster(aod2, basegrid, id = "aodid", zvar = "distedg")
# plotraster(aod3, basegrid, id = "aodid", zvar = "distedg")
