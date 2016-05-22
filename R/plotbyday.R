# reusable functions
# this file kept clean to be sourced
#################################
# function to assign closest
# merge data by day to the closest of K nearest neighbors (with a distance constraint)
# to do: (6/15/2014) add flexibility on day (name of variable - but add check on class across dt1 and dt2)
nearestbyday <- function(matrix1, matrix2, dt1, dt2, dt1varname, dt2varname, 
                         closestname = "closestmet", varstoget = "avewsp", 
                         knearest = 5, maxdistance = NA, nearestmean = FALSE){
  require(FNN)
  require(data.table)
  # to add: 
  # 1. check consistency of arguments (class of inputs, day is a date, etc)
  # 2. check that dt1varname is in dt1 and dt2varname is in dt2
  # 3. check that dt2 covers all of the needed days - otherwise error with cartesian join
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
  unique.dt <- unique(datatable[,c(xvar,yvar, idvar), with = F])
  out.m <- as.matrix(unique.dt[,c(xvar,yvar), with = F])
  dimnames(out.m)[[1]] <- unique.dt[,idvar, with = F][[1]]
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

#####################################

##############################################
# raster plot by day
library(data.table)
library(ggplot2)

# pull out a single day and
# plot with colored solid square points
if(0){
datatable <- mod3[day == as.Date("2010-07-02"),c("AOD", "x_aod_utm", "y_aod_utm"), with = F]
ggplot(datatable, aes(x_aod_utm, y_aod_utm, color = AOD)) + 
  geom_point(size = 4.15, shape = 15)
}

# raster plot by day
library(data.table)
library(ggplot2)

# pull out a single day and
# plot with colored solid square points
# function to plot by day
plotdaymex <- function(datatable, datestring, zvar = "AOD",
                       longvar = "long_aod", latvar = "lat_aod", saveplots = T) {
  myvars <- c(zvar, longvar, latvar)
  myvars <- c(myvars, colnames(datatable)[colnames(datatable) %in% c("yr", "dayofyr", "season", "month")])
  daytitle <- datestring
  # check if datestring is a date or something else
  if(inherits(try(as.Date(datestring)), "Date")){
    daydat <- datatable[day == as.Date(datestring), myvars, with = F]
  } else daydat <- datatable[, myvars, with = F]
  # set range to max extent in larger dataset
  dtrange <- data.table(x = c(min(datatable[, longvar, with = F]),
                              max(datatable[, longvar, with = F])),
                        y = c(min(datatable[, latvar, with = F]),
                              max(datatable[, latvar, with = F])),
                        z = c(0.248577))# median of mod2$AOD
  # empty plot if no data
  if(nrow(daydat) == 0){
    ggplot(dtrange, aes(x,y,colour = z)) +
      geom_blank() +
      expand_limits(colour = c(0.15, 0.35)) + 
      scale_colour_gradientn("AOD", colours=c("#FFFFD4", "#8C2D04", 'red', 'yellow'),
                             values   = c(0, 1.5, 2, 4),
                             breaks = c(seq(0, 1.5, by = 0.1), 2, 4),
                             rescaler = function(x,...) x,
                             oob      = identity) + 
      guides(color = guide_colorbar(barheight = 1)) + 
      ggtitle(paste0(daytitle, "\nno data")) +
      theme_bw(18) + theme(plot.title = element_text(size = 18))
  } else {
    ggplot(daydat,
           aes_string(x = longvar, y = latvar, color = zvar)) +
      geom_point(size = 3.45, shape = 15) +
      #scale_colour_gradient(low = "#FFFFD4", high = "#8C2D04", limits = c(0,4)) +
      scale_x_continuous(limits = range(dtrange$x)) +
      scale_y_continuous(limits = range(dtrange$y)) +
      scale_colour_gradientn(colours=c("#FFFFD4", "#8C2D04", 'red', 'yellow'),
                           values   = c(0, 1.5, 2, 4),
                           breaks = c(seq(0, 1.5, by = 0.1), 2, 4),
                           rescaler = function(x,...) x,
                           oob      = identity) + 
      guides(color = guide_colorbar(barheight = 15)) + 
      ggtitle(daytitle) +
      theme_bw(18) + theme(plot.title = element_text(size = 18))
  }
  if(saveplots){
    ggsave(file.path(getwd(), "figures", "plotdaymex", paste0("mexplot_", format(as.Date(datestring), "%Y%j"), ".pdf")),
           height = 8.5, width = 11, dpi = 96)
  } else(last_plot())
}

pause <- function(){
  cat("Pause. Press <Enter> to continue, q to stop...")
  endplots <- readline()
  invisible(endplots)
}

# this section is turned off
if(0){
  # load in raw data
  
  # print a series of days
  aodrange <- mod2[, range(AOD)]
  start <- as.Date("2010-01-01")
  for(i in 0:364){
    try(print(plotdaymex(mod2, start + i, longvar = "x_aod_utm", latvar = "y_aod_utm")))
    #endplots <- pause()
    #if(endplots == "q") stop("end of plotting")
  }
  
  # call pdftk on linux to make them into a single PDF
  system("pdftk ~/projects/airmex/figures/plotdaymex/mexplot_*.pdf cat output ~/projects/airmex/figures/mexplotmanysingledays2010.pdf")
  system(" gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE -dBATCH  -dQUIET -sOutputFile=mexplotmanysingledays2010_smaller.pdf mexplotmanysingledays2010.pdf")
  file.remove(list.files("~/projects/airmex/figures/plotdaymex/", pattern = "mexplot_.*", full.names = TRUE))

  start <- as.Date("2010-01-01")
  for(i in 1:100){
    try(print(plotdaymex(mod2, start + i, longvar = "x_aod_utm", 
                         latvar = "y_aod_utm", saveplots = F)))
    endplots <- pause()
    if(endplots == "q") stop("end of plotting")
  }
  
  
} #End of turned off


if(0){
plotdaymex(dat, "2009-02-26")
plotdaymex(dat, "2009-02-27")
plotdaymex(dat, "2012-02-28")
plotdaymex(dat, "2012-05-06")
plotdaymex(dat, "2012-10-15")
plotdaymex(dat, "2012-10-16")
plotdaymex(dat, "2008-02-28")# a non-flagged day
}#all turned off

pause <- function(){
  cat("Pause. Press <Enter> to continue, q to stop...")
  endplots <- readline()
  invisible(endplots)
}

is_simple_error <- function(x) inherits(x, "simpleError")

# End of file
