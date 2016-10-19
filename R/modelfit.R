#' Summarize linear model fit.
#'
#' @param dataframe a data.frame
#' @param yvar character vector of the column name for outcome
#' @param xvar character vector of the column name(s) for predictor(s)
#'
#' @return a data.frame of the r-squared and the rmse for the linear regression
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats resid
#' @export
modelfit <- function(dataframe, yvar = "mean_pm25", xvar = "aod47") {
  lmmod <- lm(as.formula(paste(y, "~", paste(x, collapse = "+"))), dataframe)
  data.frame(broom::glance(lmmod)[1], rmse = sqrt(mean((resid(lmmod))^2)))
}  
