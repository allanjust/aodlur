#' Merge two point datasets by day.
#'
#' Merge point datasets by day to the closest of K nearest neighbors 
#'  (with an optional distance constraint).
#'  
#' @param jointo.pts points matrix output from \code{\link{makepointsmatrix}}
#' @param joinfrom.pts points matrix output from \code{\link{makepointsmatrix}}
#' @param jointo data.table target  
#' @param joinfrom data.table data source
#' @param jointovarname column name for unique field for points in jointo
#' @param joinfromvarname column name for unique field for points in joinfrom
#' @param joinprefix string to use as name for new variables
#' @param valuefield column name with field to be merged in
#' @param knearest number of nearest neighbors to check for non-missing data
#' @param maxdistance (optional) constraint on distance, in the units of the points matrix
#' @param nearestmean (optional) also compute mean of knearest values within maxdistance?
#' @param verbose print output of the intermediate data.table size and fields
#' @return A data.table, akin to a semi_join on nearest neighbor and day.
#' @examples
#'jointo <- data.table(x = 1:5, y = 1:5, siteidx = 1:5,
#'                     day = as.Date("2004-04-01"))
#'joinfrom <- data.table(x = rep(1:5 + 0.1, times = 5),
#'                       y = rep(1:5, each = 5), 
#'                       siteidy = letters[1:25], # currently this needs to be character
#'                       value = 100 + 1:25,
#'                       day = as.Date("2004-04-01"))
#'                       
#'jointo.pt <- makepointsmatrix(datatable = jointo, 
#'                              xvar = "x", yvar = "y", idvar = "siteidx") 
#'joinfrom.pt <- makepointsmatrix(datatable = joinfrom, 
#'                                xvar = "x", yvar = "y", idvar = "siteidy") 
#'joinout <- nearestbyday(jointo.pts = jointo.pt, joinfrom.pts = joinfrom.pt, 
#'                        jointo = jointo, joinfrom = joinfrom, 
#'                        jointovarname = "siteidx", joinfromvarname = "siteidy", 
#'                        joinprefix = "nearest", valuefield = "value", 
#'                        knearest = 3, maxdistance = 2, 
#'                        nearestmean = TRUE, verbose = T)
#'joinout                        
#' @export
nearestbyday <- function(jointo.pts, joinfrom.pts, jointo, joinfrom, jointovarname, joinfromvarname, 
                         joinprefix = "closest", valuefield = "avewsp", 
                         knearest = 5, maxdistance = NA, nearestmean = FALSE, verbose = F){
  knearest <- min(knearest, nrow(joinfrom.pts))
  knnname <- paste0(joinprefix, "knn")
  # calculate nearest neighbors using package FNN
  knn_store <- get.knnx(joinfrom.pts, jointo.pts, k = knearest)
  # restrict by distance
  if(!is.na(maxdistance)){
    knn_store[["nn.dist"]][knn_store[["nn.dist"]] > maxdistance] <- NA
    knn_store[["nn.index"]] <- knn_store[["nn.index"]] * (knn_store[["nn.dist"]] * 0 + 1)
  }
  # store the indices for nearest neighbors in a long DT
  knn_out <- data.table(matrix(knn_store[["nn.index"]])) 
  knn_out[, jointovarname := rep(rownames(jointo.pts), knearest), with = F]
  knn_out[, joinprefix := row.names(joinfrom.pts[knn_out[, V1],]), with = F]
  knn_out[, V1 := NULL]
  knn_out[, knnname := rep(1:knearest, each = nrow(jointo.pts)), with = F]
  # drop points not within maxdistance
  knn_out <- knn_out[!is.na(get(joinprefix))]
  # use setkeyv to pass a column by name
  setkeyv(knn_out, joinprefix)
  setnames(joinfrom, joinfromvarname, joinprefix)
  # if not character - coerce
  if(class(joinfrom[,joinprefix,with = F][[1]]) != "character"){
    joinfrom[, joinprefix := as.character(joinprefix), with = F]
  }
  # since jointovarname came through matrix rownames in jointo.pts it was coerced to character in joinfromlong above
  jointo[, jointovarname := as.character(get(jointovarname)), with = F]
  setkeyv(jointo, cols = c("day"))
  setkeyv(joinfrom, "day")
  # only retain days in joinfrom from jointo
  # but need to rename so that we can reset the name in joinfrom below
  joinfromsub <- joinfrom[J(unique(jointo[,day]))]
  setkeyv(joinfromsub, joinprefix)
  # lengthen joinfrom with every possible site each day might match
  # after dropping missing observations
  joinfromlong <- joinfromsub[!is.na(get(valuefield))][knn_out, allow.cartesian = T]
  if(verbose) {(print(paste0("cartesian join leads to joinfromlong with ", 
               format(nrow(joinfromlong), big.mark = ",", scientific = F), 
               " rows")))}
  # store the number of valid observations and mean - costly! this is optional
  if(nearestmean){
    nobsname <- paste0(joinprefix, "nobs")
    nearestmeanname <- paste0(joinprefix, "mean")
    newfields <- c(nobsname, nearestmeanname)
    joinfromlong[, newfields := list(.N, mean(get(valuefield))), 
            by=c(jointovarname,"day"), with = F]
  }
  # shouldn't Gforce have sped this up in data.table 1.9.2? 
  # doesn't appear faster than with options(datatable.optimize=1) #turn off Gforce
  # only use days in both jointo and joinfrom
  setkeyv(jointo, cols = c(jointovarname, "day"))
  setkeyv(joinfromlong, cols = c(jointovarname, "day", knnname))
  # join joinfromlong to first 2 keys (dtvar1name and day in jointo)
  # and take first record (closest) for fast selection
  closestvar <- joinfromlong[jointo[, c(jointovarname, "day"), with = F], mult = "first"]
  # put the name back in joinfrom
  setnames(joinfrom, joinprefix, joinfromvarname)
  # inspect our result
  if(verbose) print(tables(silent = T)[NAME == "closestvar"])
  # return it silently
  invisible(closestvar)
}