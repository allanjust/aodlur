#' Summarize linear model fit.
#'
#' @param dataframe a data.frame
#' @param yvar character vector of the column name for outcome
#' @param xvar character vector of the column name(s) for predictor(s)
#' @param digits integer number of digits to print
#' @param verbose logical whether to print summary
#' @return a data.frame of the r-squared and the rmse for the linear regression
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats resid
#' @examples
#'test <- data.frame(x = 1:100, y = 1:100 + rnorm(100))
#'modelfit(test, yvar = "y", xvar = "x")
#' @export
modelfit <- function(dataframe, yvar = "pm", xvar = "aod47", digits = 2, verbose = TRUE) {
  lmmod <- lm(as.formula(paste(yvar, "~", paste(xvar, collapse = "+"))), dataframe)
  outdf <- data.frame(broom::glance(lmmod)[1], 
                      rmse = sqrt(mean((resid(lmmod))^2)), 
                      n = length(resid(lmmod)))
  if(verbose) print(outdf, digits = digits)
  invisible(outdf)
}  
