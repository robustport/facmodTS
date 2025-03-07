#' @title Fit Returns to FF3 Model
#' 
#' @description Fits univariate xts returns to a Fama-French 3-factor data
#' set consisting of market excess returns (MKT), the small-minus-big (SMB)
#'  factor, and the high-minus low (HML) factor
#' 
#' @param returns Univariate xts returns data set
#' @param datFF Trivariate xts FF3 data set
#' @param digits Integer number of significant digits, with default digits = 2
#' @param title Character optional plot title, default = NULL
#'
#' @returns List containing a data frame LSRobFit and a data frame pvals
#' 
#' @details The LSRobFit data frame contains the Alpha (intercept) MKT, SMB, HML
#' least squares and mOPt robust coefficient estimates, with their t-values in
#' parentheses, and the LS and robust adjusted R-squared values (AdjRSQ).  The
#' p-values data frame contains p-values of significance tests of robust versus
#' LS estimates for the overall model, and for the MKT, SMB, and HML factors
#' separately. The significance tests are computed with the lsRobTestMM.R
#' function in the PCRA package.
#' 
#' @export
#'
#' @examples
#' args(fitReturnsToFF3model)
fitReturnsToFF3model <- function(returns,datFF,digits = 2,title = NULL)
{
  reg <- cbind(returns,datFF)
  names(reg) <- c("RET",names(reg)[2:4])
  #tsPlotMP(reg)
  regdf <- as.data.frame(reg)
  fitLS <- lm(RET ~ .,data = regdf)
  fitRob <- RobStatTM::lmrobdetMM(RET~.,data = regdf)
  fitRobLS <- fit.models::fit.models(fitLS,fitRob)
  # sideBySideQQPlot(fitRobLS,fun = residuals,main = title,xlab = "Standard Normal Quantiles",
  #                 ylab = "Ordered Residuals")
  tLS = sapply(summary(fitLS)$coefficients[, 3], function(x) paste("(", round(x,2), ")", sep=""))
  LS <- c(paste0(round(coef(fitLS), digits), tLS), round(summary(fitLS)$adj.r.squared, digits))
  tRob = sapply(summary(fitRob)$coefficients[, 3], function(x) paste("(", round(x,2), ")", sep=""))
  Robust <- c(paste0(round(coef(fitRob),digits), tRob), round(summary(fitRob)$adj.r.squared, digits))
  LSRobFit <- data.frame(rbind(LS,Robust))
  names(LSRobFit) <- c("Alpha", names(reg)[2:4], "AdjRSQ")
  
  test <- lsRobTestMM(fitRob)
  MODEL <- round(test$full$p.value,3)
  pvalCoefs <- round(t(test$coefs[,6]),3)
  pvals <- data.frame(cbind(MODEL,pvalCoefs))
  row.names(pvals) <- "p-Values"
  list("LSRobfit" = LSRobFit,"pvalsCompare" = pvals)
}
