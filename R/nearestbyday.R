
# function to assign closest value across grids
# merge data by day to the closest of K nearest neighbors (with a distance constraint)
nearestbyday <- function(matrix1, matrix2, dt1, dt2, dt1varname, dt2varname, 
                         closestname = "closestmet", varstoget = "avewsp", 
                         knearest = 5, maxdistance = NA, nearestmean = FALSE){
  require(FNN)
  require(data.table)
  ## to add: 
  # 0.0 create a simple test of the function
  # 0.05 consistent naming of variables to "jointo" and "joinfrom"
  # 0.1 bring call to makepointsmatrix in to the function (currently done ahead of time)
  # 0.2 easier spatial-only merge as an option
  # 0.3 change closestname to "joinprefix"
  # 1. check consistency of arguments (class of inputs, day is a date, etc)
  # 2. check that dt1varname is in dt1 and dt2varname is in dt2
  # 3. check that dt2 covers all of the needed days - otherwise error with cartesian join
  # 4. add flexibility on "day" (name of variable - but add check on class across dt1 and dt2)
  # 4.1 add parameter "datename" instead of assuming it is called day
  # 5. add flag to exclude closest point in calculating mean (for generating a second feature)

  knearest <- min(knearest, nrow(matrix2))
  knnname <- paste0(closestname, "knn")
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
  knn_out[, closestname := as.character(row.names(matrix2[knn_out[, V1],])), with = F]
  knn_out[, V1 := NULL]
  knn_out[, knnname := rep(1:knearest, each = nrow(matrix1)), with = F]
  # drop points not within maxdistance
  knn_out <- knn_out[!is.na(get(closestname))]
  # use setkeyv to pass a column by name
  setkeyv(knn_out, closestname)
  setnames(dt2, dt2varname, closestname)
  # if not character - coerce
  if(class(dt2[,closestname,with = F][[1]]) != "character"){
    dt2[, closestname := as.character(closestname), with = F]
  }
  # since dt1varname came through matrix rownames in matrix1 it was coerced to character in dt2long above
  dt1[, dt1varname := as.character(get(dt1varname)), with = F]
  setkeyv(dt1, cols = c("day"))
  setkeyv(dt2, "day")
  # only retain days in dt2 from dt1
  # but need to rename so that we can reset the name in dt2 below
  dt2sub <- dt2[J(unique(dt1[,day]))]
  setkeyv(dt2sub, closestname)
  # lengthen dt2 with every possible site each day might match
  # after dropping missing observations
  dt2long <- dt2sub[!is.na(get(varstoget))][knn_out, allow.cartesian = T]
  print(paste0("cartesian join leads to dt2long with ", 
               format(nrow(dt2long), big.mark = ",", scientific = F), 
               " rows"))
  # store the number of valid observations and mean - costly! this is optional
  if(nearestmean){
    nobsname <- paste0(closestname, "nobs")
    nearestmeanname <- paste0(closestname, "mean")
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
  setnames(dt2, closestname, dt2varname)
  # inspect our result
  print(tables(silent = T)[NAME == "closestvar"])
  # return it silently
  invisible(closestvar)
}


makepointsmatrix <- function(datatable, xvar, yvar, idvar) {
  dtnames <- names(datatable)
  unique.dt <- unique(datatable[, c(xvar, yvar, idvar), with = F])
  out.m <- as.matrix(unique.dt[, c(xvar, yvar), with = F])
  dimnames(out.m)[[1]] <- unique.dt[,idvar, with = F][[1]]
  # check if any missing coordinates
  if(anyNA(out.m)){
    warning("missing values in x or y coordinates")
  }
  invisible(out.m)
}

# example code for spatial-temporal join
if(0){
# to each pm monitor - join in the closest temp data
closesttemp <- nearestbyday(pm_mons.m, met_mons.m, 
                            dat.gis, all.met[,c("day", "mon", "tempmean", "tempdaynum"), with = F], 
                            "mon", "mon", "closesttemp", "tempmean", knearest = 25)
closestrh <- nearestbyday(pm_mons.m, met_mons.m, 
                          dat.gis, all.met[,c("day", "mon", "rhmean", "rhdaynum"), with = F],  
                          "mon", "mon", "closestrh", "rhmean", knearest = 25)
closestavewsp <- nearestbyday(pm_mons.m, met_mons.m, 
                              dat.gis, all.met[,c("day", "mon", "avewsp", "avewdr", "nave", "eave", "winddaynum"), with = F],  
                              "mon", "mon", "closestwind", c("avewsp", "avewdr"), knearest = 25)

setkey(closestavewsp, mon, day)
setkey(dat.gis, mon, day)
# not quite sure how best to merge this back
closestavewsp[dat.gis, daymean := daymean]
}# End of turned off

# End of file
