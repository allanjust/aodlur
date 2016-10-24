#' Summarize linear model fit.
#'
#' @param DF a data.frame
#' @param yvar character vector of the column name for outcome
#' @param xvar character vector of the column name(s) for predictor(s)
#' @param verbose logical whether to print summary
#' @param digits integer number of digits to print (if verbose is equal to FALSE, this parameter is not considered)
#' @return a data.frame of the r-squared and the rmse for the linear regression
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats resid
#' @examples
#' test <- data.frame(x = 1:100, y = 1:100 + rnorm(100))
#' modelfit(test, yvar = "y", xvar = "x")
#' @export

modelfit <- function(DF, yvar = "pm", xvar = "aod47", 
                     verbose = TRUE, digits = 2) {
  if (sum(!is.na(DF[, xvar])) == 0 | sum(!is.na(DF[, yvar])) == 0) {
    outdf <- data.frame(r.squared = NA_real_, 
                        rmse = NA_real_, 
                        n = NA_integer_)  
  } else {
    lmmod <- try(lm(as.formula(paste(yvar, "~", paste(xvar, collapse = "+"))), DF))
    if (class(lmmod) == "try-error") {
      outdf <- data.frame(r.squared = NA_real_, 
                          rmse = NA_real_, 
                          n = NA_integer_)  
    } else {
      rsquareddf <- try(broom::glance(lmmod)[1])
      if (class(rsquareddf) == "try-error") {
        outdf <- data.frame(r.squared = NA_real_, 
                            rmse = NA_real_, 
                            n = NA_integer_) 
      } else {
        outdf <- data.frame(rsquareddf, 
                            rmse = sqrt(mean((resid(lmmod))^2)), 
                            n = length(resid(lmmod)))
      }}}
  if (verbose) print(outdf, digits = digits) 
  invisible(outdf)
}  
