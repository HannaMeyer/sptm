#' calculate prediction performance statistics for binary classification models
#'
#' @description
#' this function calculates prediction performance statistics
#' between vectors of predicted and observed values.
#'
#' @param prd factor vector of predicted values with two levels
#' @param obs factor vector of observed values with two levels
#' @param prob optional. Predicted probabilities for the first class
#'
#' @return data frame containing the classification statistics.
#' @author
#' Hanna Meyer
#'
#' @examples
#' #create two random vectors with classes "yes" and "no" to simulate a model
#' #with random performance. Expected POD and PFD =50%
#' pred_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#' obs_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#'
#' result <- classificationStats(pred_vals, obs_vals)
#' result
#'
#' @export classificationStats
#' @aliases classificationStats
#' @seealso \code{\link{regressionStats}}

classificationStats <- function(prd, obs, prob=NULL) {
  tab <- (table(prd,obs))/100
  TP <- tab[1,1]
  FP <- tab[1,2]
  TN <- tab[2,2]
  FN <- tab[2,1]

  bias <- (TP+FP)/(TP+FN)
  POD <- TP/(TP+FN)
  PFD <- FP/(FP+TN)
  FAR <- FP/(TP+FP)
  CSI <- TP/(TP+FP+FN)
  ph <- ((TP+FN)*(TP+FP))/(sum(tab))
  ETS <- (TP-ph)/((TP+FP+FN)-ph)
  HSS <- (TP*TN-FP*FN)/(((TP+FN)*(FN+TN)+(TP+FP)*(FP+TN))/2)
  HKD <- (TP/(TP+FN))-(FP/(FP+TN))
  if (!is.null(prob)){
    AUC <- as.numeric(pROC::roc(obs,prob)$auc)
    df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD,AUC)
    names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD","AUC")
  }else{
    df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD)
    names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD")
  }
  return(df_all)
}
