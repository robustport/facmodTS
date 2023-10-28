#' @keywords internal
#' @import data.table
#' @importFrom grDevices topo.colors
#' @importFrom graphics abline arrows axis barplot box boxplot identify legend lines mtext par points polygon segments strheight strwidth text title
#' @importFrom methods cbind2
#' @importFrom stats cov dnorm integrate lm mad median pnorm qchisq sd uniroot var qnorm qqnorm
#' @importFrom utils globalVariables download.file menu
#' @importFrom lattice panel.abline xyplot panel.xyplot
#' @importFrom xts xts as.xts apply.monthly
#' @importFrom zoo zoo coredata index index<-
#' @importFrom boot boot
#' @importFrom PerformanceAnalytics checkData		
#' @importFrom PortfolioAnalytics create.EfficientFrontier		
#' @importFrom corpcor make.positive.definite
#' @importFrom quadprog solve.QP
#' @importFrom RobStatTM locScaleM lmrobdet.control lmrobdetMM
#' @importFrom robustbase scaleTau2
#' @importFrom R.cache loadCache saveCache
#' @importFrom graphics panel.smooth rug
#' @importFrom grDevices dev.off
#' @importFrom stats as.formula coef cor cov2cor density fitted
#' @importFrom stats fitted formula hatvalues lag na.omit 
#' @importFrom stats printCoefmat quantile residuals time

"_PACKAGE"

globalVariables(c("Date"))