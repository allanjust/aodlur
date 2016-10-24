#' Construct points matrix.
#'
#' @param datatable a data.table
#' @param xvar column name for x-coordinate
#' @param yvar column name for y-coordinate
#' @param idvar column name for unique key; currently better to use 
#' character (gets stored as row.names)
#'
#' @return a matrix of unique x, y pairs with rownames.
#' @export 
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