#' @title Fitted TSFM Model Statistics
#' 
#' @description
#' Computes fitted time series factor model (TSFM) models standard errors,
#' t-statistics, R-squared and adjusted R-squared values
#' 
#'
#' @param fit A TSFM fitted model object
#' @param digits Integer number of digits for rounding, with default digits = 2 
#'
#' @returns A fitted TSFM object
#' @export
#'
#' @examples
#' args(fitTsfmStats)
fitTsfmStats <- function(fit, digits = 2)
{
  # Create data frame for the estimates, and their
  # standard errors and t-statistics
  fitSum <- summary(fit)
  coefs <- t(fitSum$sum.list[[1]]$coefficients)
  coefs <- data.frame(round(coefs[1:3, ], digits))
  names(coefs)[1] <- "Intercept"
  row.names(coefs) <- c("Estimates", "Std.Errors",
                        "t.Stats")
  
  # Add R-Squared and Adjusted R-Squared Rows
  rsq <- fitSum$sum.list[[1]]$r.squared
  adj.rsq <- fitSum$sum.list[[1]]$adj.r.squared
  adj.rsq <- round(adj.rsq, digits)
  n.fac <- length(fit$factor.names)
  adjRsqCol <- c(adj.rsq, "", "")
  tbl <- cbind(coefs, adjRsqCol)
  n <- length(names(tbl))
  names(tbl)[n] <- "Adj.Rsq"
  return(tbl)
}