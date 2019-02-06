#' Geometric mean for model evaluation
#' @description Provides functionality to evaluate a model using the gemetric mean as described in Galar et al., 2014.
#' The function can be used in caret's \code{\link{train}} and CAST'S \code{\link{ffs}} to select variables based on the geometric mean.
#' @param data a data frame with columns obs and pred for the observed and predicted outcomes.
#' @param lev a character vector of factors levels for the response.
#' @param model a character string for the model name (as taken from the method argument of \code{\link{train}}.
#' @return A vector of geometric mean performance estimates.
#' @author Hanna Meyer, Jannis Gottwald, Thomas Nauß
#' @seealso \code{\link{train}},code{\link{trainControl}},\code{\link{ffs}},
#' \code{\link{defaultSummary}}
#' @references
#' \itemize{
#' \item Galar, M., Fernandez, A., Barrenechea, E., Herrera, F. (2014): Empowering difficult classes with a similarity-based aggregation in multi-class classification problems. Information Sciences 264: 135-157.
#' }
#' @examples
#' \dontrun{
#' data(iris)
#' ctrl <- trainControl(method="cv",summaryFunction = GMSummary)
#' model <- train(iris[,1:4],iris$Species,trControl=ctrl,metric="GM")
#' model
#'}
#' @export GMSummary
#' @aliases GMSummary

GMSummary <- function (data,
                       lev = NULL,
                       model = NULL){

  prd <- as.character(data$pred)
  obs <- as.character(data$obs)
  TP <- c()
  TP <- lapply(seq(length(unique(obs))), function(i){
    obs_recl <- obs
    prd_recl <- prd
    # reclassify each class into binary (yes/no)
    obs_recl[obs_recl!=unique(obs)[i]] <- "No"
    #print(prd_recl[prd_recl!=unique(obs)[i]])
    prd_recl[prd_recl!=unique(obs)[i]] <- "No"

    #calculate True positive rate from cross-table
    if(length(unique(prd_recl)) == 1){
      tp = 0.0
    } else {
      tab <- table(prd_recl,obs_recl)
      tp <- tab[1,1]/(tab[1,1]+tab[2,1])
    }
    return(tp)
  })
  TP <- unlist(TP)
  # Function for GM according to Galar et al 2014 (maths implementation correct here?)
  GM <- prod(TP)^(1/length(unique(obs)))
  names(GM) <- "GM"
  return(GM)
}
