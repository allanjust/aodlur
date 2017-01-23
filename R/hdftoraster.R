#' Import an hdf file into a RasterLayer object.
#'
#' @param path path of the hdf file
#' @param n number of the file the user wants to import  
#' 
#' @return a RasterLayer object.
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster raster
#' @export
hdftoraster <- function(path, n) {
  sds = get_subdatasets(path)
  x = strsplit(sds[n], ":")[[1]]
  x = paste0(x[1], ":", x[2], ':"', x[3], '":', x[4], ":", x[5], ":", x[6])
  system(
    paste0(
      "gdal_translate -of GTiff ", "\"", x, "\" ",
      tempdir(), gsub(".hdf", paste0("_", n, ".tif"), paste0("/", gsub(".*/", "", path)))
    )
  )
  return(raster(paste0(tempdir(), gsub(".hdf", paste0("_", n, ".tif"), 
                                       paste0("/", gsub(".*/", "", path)))))) # convert to raster
  
  file.remove(paste0(tempdir(), gsub(".hdf", paste0("_", n, ".tif"), 
                                     paste0("/", gsub(".*/", "", path))))) # remove the tif
}
