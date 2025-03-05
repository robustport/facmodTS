#' Title Fitted TSFM Single Factor Model Statistics
#' 
#' @description
#' Computes fitted time series single factor model t-statistics, and 
#' adjusted R-squared values
#'
#' @param fit A fitted TSFM for a single factor 
#' @param digits Integer number of digits for rounding, with default digits = 2 
#'
#' @returns A fitted TSFM object for a single factor
#' @export
#'
#' @examples
#'  args(fitTsfmSingleFactorStats)
fitTsfmSingleFactorStats <- function(fit, digits = 2)
{
  # Single factor model t-stats and R-squared
  digits <- digits
  fitSum <- summary(fit)
  coefs <- t(fitSum$sum.list[[1]]$coefficients)
  coefs <- data.frame(round(coefs, digits))
  names(coefs)[1] <- "Alpha"
  t.vals1 <- coefs[3,1]
  t.vals2 <- coefs[3,2]
  Rsq <- round(fitSum$sum.list[[1]]$adj.r.squared, digits)
  tbl <- data.frame(coefs[1,1], t.vals1, 
                    coefs[1,2], t.vals2, Rsq)
  names(tbl) <- c(names(coefs)[1], "t-Stat",
                  names(coefs)[2], "t-Stat", "Adj R-Sq")
  row.names(tbl) <- "Estimates"
  tbl
}