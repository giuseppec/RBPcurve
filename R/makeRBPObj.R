#' @title Create data container for RBP curve.
#'
#' @description
#' Must be created for all subsequent plot function calls. 
#'
#' @param pred [\code{numeric}]\cr
#'   Predicted probabilities for each observation.
#' @param y [\code{numeric} | \code{factor}]\cr
#'   Class labels of the target variable. 
#'   Either a numeric vector with values \code{0} or \code{1}, or a factor with two levels.
#' @param positive [\code{character(1)}]\cr
#'  Set positive class label for target variable which is transformed as \code{1} to compute.
#'  Only present when \code{y} is a "factor".
#' 
#' @return
#' Object members:
#' \describe{
#'   \item{\code{n} [\code{numeric(1)}]}{Number of observations.}
#'   \item{\code{pred} [\code{numeric(n)}]}{Predicted probabilities.}
#'   \item{\code{y} [\code{numeric(n)}]}{Target variable having the values 0 and 1.}
#'   \item{\code{positive} [\code{character(1)}]}{Positive class label of traget variable. Only present when \code{y} is a factor.}
#'   \item{\code{e0} [\code{numeric(1)}]}{Average of the predicted probabilities conditional on \code{y=0}.}
#'   \item{\code{e1} [\code{numeric(1)}]}{Average of the predicted probabilities conditional on \code{y=1}.}
#'   \item{\code{pev} [\code{numeric(1)}]}{Proportion of explained variation measure. Computed as \code{e1-e0}.}
#'   \item{\code{tpr} [\code{numeric(1)}]}{True positive rate.}
#'   \item{\code{fpr} [\code{numeric(1)}]}{False positive rate.}
#'   \item{\code{prevalence} [\code{numeric(1)}]}{Prevalence.}
#'   \item{\code{oneMinusPrev} [\code{numeric(1)}]}{One minus the value of the prevalence.}
#'   \item{\code{axis.x} [\code{numeric(n)}]}{Values for the X-Axis of the RBP curve.}
#'   \item{\code{axis.y} [\code{numeric(n)}]}{Values for the Y-Axis of the RBP curve.}
#'  }
#' 
#' @export
#' @import BBmisc
#' @aliases RBPObj
makeRBPObj = function(pred, y, positive = NULL) {
  
  # Check and convert arguments
  assertNumeric(pred)
  assert(
    checkNumeric(y, len = length(pred), any.missing = FALSE),
    checkFactor(y, len = length(pred), any.missing = FALSE)
  )
  if (is.factor(y)) {
    if (is.null(positive)) {
      positive = levels(y)[1L]
    } else assertSubset(positive, levels(y))
    y = as.numeric(y == positive)
  } else {
    assertSubset(y, c(0, 1))
    if (!is.null(positive)) assertNull(positive)
  }
  
  # Compute several measures
  n = length(y)
  eps = y - pred
  prevalence = mean(y)
  tpr = mean(pred[y == 1] > prevalence)
  fpr = mean(pred[y == 0] > prevalence)
  e0 = mean(pred[y == 0])
  e1 = mean(pred[y == 1])
  
  # Computes x and y axis for RBP curve
  axis.x = (1:n) / n
  axis.y = sort(eps)
  
  # Get first value below and first value above horizontal line at 0
  #below0 = max(which(axis.y < 0))
  #above0 = min(which(axis.y > 0))
  
  # Get the value of the x-axis where the RBP curve intersects the horizontal line at 0
  #interpol = approx(axis.x[c(below0, above0)}], axis.y[c(below0, above0)}], n = 1000)
  #vertical.line = interpol$x[which.min(abs(interpol$y))}]
  
  makeS3Obj("RBPObj",
    n = n,
    pred = pred,
    y = y,
    positive = positive,
    e0 = e0,
    e1 = e1,
    pev = e1 - e0,
    tpr = tpr,
    fpr = fpr,
    prevalence = prevalence,
    oneMinusPrev = 1 - prevalence,
    axis.x = axis.x,
    axis.y = axis.y
  )
}


