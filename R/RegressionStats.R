#' calculate prediction performance statistics of regression models
#'
#' @description
#' this function calculates prediction performance statistics
#' between vectors of predicted and observed values, namely
#' coefficient of determination (Rsq), root mean squared error (RMSE),
#' mean error (ME), mean absolute error (MAE).
#'
#' @param prd numeric vector of predicted values
#' @param obs numeric vector of observed values
#' @param adj.rsq logical, whether to return adjusted r-squared.
#' Defaults to TRUE
#' @param method character. Method to use for correlation. See \code{?cor.test}
#' for details.
#'
#' @return a data frame.

#'
#' @author
#' Tim Appelhans, Hanna Meyer
#'
#' @examples
#' ## create predictions with high accuracy (identical mean),
#' ## but low precision (sd double that of observations). Hence,
#' ## ME should be close to zero and RMSE close to ten.
#' pred_vals <- sort(rnorm(1000, 200, 20)) # sorting ensures high Rsq
#' obs_vals <- sort(rnorm(1000, 200, 10))
#' result <- regressionStats(pred_vals, obs_vals, adj.rsq = FALSE)
#' result
#'
#' @export regressionStats
#' @aliases regressionStats

regressionStats <- function(prd, obs, adj.rsq = TRUE,
                            method="pearson") {

  mod <- lm(prd ~ obs)
  if(adj.rsq) rsq <- summary(mod)$adj.r.squared else
    rsq <- summary(mod)$r.squared
  cor <- cor.test(obs,prd,method=method,exact=FALSE)$estimate

  df_all <- data.frame(ME = mean(prd - obs, na.rm = TRUE),
                       ME.se = se(prd - obs),
                       MAE = mean(abs(prd - obs), na.rm = TRUE),
                       MAE.se = se(abs(prd - obs)),
                       RMSE = sqrt(mean((prd - obs)^2, na.rm = TRUE)),
                       RMSE.se = se((prd - obs)^2),
                       Rsq = rsq,
                       cor=cor[[1]])
  names(df_all)[names(df_all)=="cor"] <- names(cor) #adapt rho/tau depending on method
  return(df_all)
}
