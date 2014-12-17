#' @title Create data container for RBP curve.
#'
#' @description
#' Must be created for all subsequent plot function calls. \cr
#' Object members:
#' 
#' 
#' 
#' 
#'
#' @param pred [\code{numeric}]\cr
#'   Predicted probabilities for each observation.
#' @param y [\code{numeric} | \code{factor}]\cr
#'   Class labels of the target variable. 
#'   Either a numeric vector with values \code{0} or \code{1}, or a factor with two levels.
#' @param positive [\code{character}(1)]\cr
#'  Set positive class label for target variable which is transformed as \code{1} to compute.
#'  Only present when \code{y} is a "factor".
#' 
#' @export
#' @import BBmisc
#' @aliases RBPObj
makeRBPObj = function(pred, y, positive = NULL) {
  # check and convert arguments
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
    assertSubset(y, c(0,1))
    if(!is.null(positive)) assertNull(positive)
  }
  
  # compute several measures
  n = length(y)
  eps = y - pred
  #ymean = mean(y)
  prevalence =  mean(y)
  tpr = mean(pred[y == 1] > prevalence)
  fpr = mean(pred[y == 0] > prevalence)
  e0 = mean(pred[y == 0])
  e1 = mean(pred[y == 1])
  
  
  # computes x and y axis for RBP curve
  axis.x = (1:n) / n
  axis.y = sort(eps)
  
  # get first value below and first value above horizontal line at 0
  #below0 = max(which(axis.y < 0))
  #above0 = min(which(axis.y > 0))
  
  # get the value of the x-axis where the RBP curve intersects the horizontal line at 0
  #interpol = approx(axis.x[c(below0, above0)], axis.y[c(below0, above0)], n = 1000)
  #vertical.line = interpol$x[which.min(abs(interpol$y))]
  
  makeS3Obj("RBPObj",
            n = n,
            pred = pred,
            y = y,
            #positive = positive,
            #ymean = ymean,
            #eps = eps,
            e0 = e0,
            e1 = e1,
            pev = e1 - e0,
            tpr = tpr,
            fpr = fpr,
            prevalence = prevalence,
            oneMinusPrev = 1-prevalence,
            axis.x = axis.x,
            axis.y = axis.y#,
            #below0 = below0,
            #above0 = above0,
            #interpol = interpol
            #vertical.line = vertical.line
  )
}


