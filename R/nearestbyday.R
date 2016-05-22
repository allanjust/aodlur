#' Merge two point datasets by day.
#'
#' Merge point datasets by day to the closest of K nearest neighbors 
#'  (with an optional distance constraint).
#' @param matrix1 
#' @param matrix2, 
#' @param dt1, 
#' @param dt2, 
#' @param dt1varname 
#' @param dt2varname
#' @param joinprefix string to use as name for new variables
#' @param varstoget
#' @param knearest number of nearest neighbors to check for non-missing data
#' @param maxdistance (optional) constraint on distance
#' @param nearestmean (optional) compute mean of knearest values within maxdistance?
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
#'joinout <- nearestbyday(matrix1 = jointo.pt, matrix2 = joinfrom.pt, 
#'                        dt1 = jointo, dt2 = joinfrom, 
#'                        dt1varname = "siteidx", dt2varname = "siteidy", 
#'                        joinprefix = "nearest", varstoget = "value", 
#'                        knearest = 3, maxdistance = 2, nearestmean = TRUE)
#setnames(joinfrom, "nearest", "siteidy") # if things fail, we have to reset the variable name manually
#' @export
nearestbyday <- function(matrix1, matrix2, dt1, dt2, dt1varname, dt2varname, 
                         joinprefix = "closest", varstoget = "avewsp", 
                         knearest = 5, maxdistance = NA, nearestmean = FALSE){
  ## to add:
  # 0.05 consistent naming of variables to "jointo" and "joinfrom"
  # 0.1 bring call to makepointsmatrix in to the function (currently done ahead of time)
  # 0.2 easier spatial-only merge as an option
  # 0.3 dt2varname shouldn't have to be a particular class (currently problems if not character)
  # 1. check consistency of arguments (class of inputs, day is a date, etc)
  # 2. check that dt1varname is in dt1 and dt2varname is in dt2
  # 3. check that dt2 covers all of the needed days - otherwise error with cartesian join
  # 4. add flexibility on "day" (name of variable - but add check on class across dt1 and dt2)
  # 4.1 add parameter "datename" instead of assuming it is called day
  # 5. add flag to exclude closest point in calculating mean (for generating a second feature)

  knearest <- min(knearest, nrow(matrix2))
  knnname <- paste0(joinprefix, "knn")
  # calculate nearest neighbors using package FNN
  knn_store <- get.knnx(matrix2, matrix1, k = knearest)
  # restrict by distance
  if(!is.na(maxdistance)){
    knn_store[["nn.dist"]][knn_store[["nn.dist"]] > maxdistance] <- NA
    knn_store[["nn.index"]] <- knn_store[["nn.index"]] * (knn_store[["nn.dist"]] * 0 + 1)
  }
  # store the indices for nearest neighbors in a long DT
  knn_out <- data.table(matrix(knn_store[["nn.index"]])) 
  knn_out[, dt1varname := rep(rownames(matrix1), knearest), with = F]
  knn_out[, joinprefix := row.names(matrix2[knn_out[, V1],]), with = F]
  knn_out[, V1 := NULL]
  knn_out[, knnname := rep(1:knearest, each = nrow(matrix1)), with = F]
  # drop points not within maxdistance
  knn_out <- knn_out[!is.na(get(joinprefix))]
  # use setkeyv to pass a column by name
  setkeyv(knn_out, joinprefix)
  setnames(dt2, dt2varname, joinprefix)
  # if not character - coerce
  if(class(dt2[,joinprefix,with = F][[1]]) != "character"){
    dt2[, joinprefix := as.character(joinprefix), with = F]
  }
  # since dt1varname came through matrix rownames in matrix1 it was coerced to character in dt2long above
  dt1[, dt1varname := as.character(get(dt1varname)), with = F]
  setkeyv(dt1, cols = c("day"))
  setkeyv(dt2, "day")
  # only retain days in dt2 from dt1
  # but need to rename so that we can reset the name in dt2 below
  dt2sub <- dt2[J(unique(dt1[,day]))]
  setkeyv(dt2sub, joinprefix)
  # lengthen dt2 with every possible site each day might match
  # after dropping missing observations
  dt2long <- dt2sub[!is.na(get(varstoget))][knn_out, allow.cartesian = T]
  print(paste0("cartesian join leads to dt2long with ", 
               format(nrow(dt2long), big.mark = ",", scientific = F), 
               " rows"))
  # store the number of valid observations and mean - costly! this is optional
  if(nearestmean){
    nobsname <- paste0(joinprefix, "nobs")
    nearestmeanname <- paste0(joinprefix, "mean")
    newfields <- c(nobsname, nearestmeanname)
    dt2long[, newfields := list(.N, mean(get(varstoget))), 
            by=c(dt1varname,"day"), with = F]
  }
  # shouldn't Gforce have sped this up in data.table 1.9.2? 
  # doesn't appear faster than with options(datatable.optimize=1) #turn off Gforce
  # only use days in both dt1 and dt2
  setkeyv(dt1, cols = c(dt1varname, "day"))
  setkeyv(dt2long, cols = c(dt1varname, "day", knnname))
  # join dt2long to first 2 keys (dtvar1name and day in dt1)
  # and take first record (closest) for fast selection
  closestvar <- dt2long[dt1[, c(dt1varname, "day"), with = F], mult = "first"]
  # put the name back in dt2
  setnames(dt2, joinprefix, dt2varname)
  # inspect our result
  print(tables(silent = T)[NAME == "closestvar"])
  # return it silently
  invisible(closestvar)
}