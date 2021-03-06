% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BinaryClassificationStats.R
\name{classificationStats}
\alias{classificationStats}
\title{calculate prediction performance statistics for binary classification models}
\usage{
classificationStats(prd, obs, prob = NULL)
}
\arguments{
\item{prd}{factor vector of predicted values with two levels}

\item{obs}{factor vector of observed values with two levels}

\item{prob}{optional. Predicted probabilities for the first class}
}
\value{
data frame containing the classification statistics.
}
\description{
this function calculates prediction performance statistics
between vectors of predicted and observed values.
}
\examples{
#create two random vectors with classes "yes" and "no" to simulate a model
#with random performance. Expected POD and PFD =50\%
pred_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
obs_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))

result <- classificationStats(pred_vals, obs_vals)
result

}
\seealso{
\code{\link{regressionStats}}
}
\author{
Hanna Meyer
}
