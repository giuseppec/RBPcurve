#' @title Create data container for RBP curve.
#'
#' @description
#' Must be created for all subsequent plot function calls.
#'
#' @export
#' @aliases RBPObj
makeRBPObj = function(pred, y, positive = NULL) {
  assertNumeric(pred)
  assert(
    checkNumeric(y, len = length(pred), any.missing = FALSE),
    checkFactor(y, len = length(pred), any.missing = FALSE)
  )
  if (is.factor(y)) {
    y = as.numeric(y == positive)
    if (is.null(positive))
      positive = levels(y)[1L]
  }

  n = length(y)
  eps = y - pred
  ymean = mean(y)
  tpr = mean(pred[y == 1] > ymean)
  fpr = mean(pred[y == 0] > ymean)
  e0 = mean(pred[y == 0])
  e1 = mean(pred[y == 1])
  prev = 1 - ymean

  # computes x and y axis for plot
  axis.x = (1:n) / n
  axis.y = sort(eps)
  below0 = max(which(axis.y < 0))
  above0 = min(which(axis.y > 0))
  interpol = approx(axis.x[c(below0, above0)], axis.y[c(below0, above0)], n = 1000)

  makeS3Obj("RBPObj",
    n = n,
    pred = pred,
    y = y,
    positive = positive,
    ymean = ymean,
    eps = eps,
    e0 = e0,
    e1 = e1,
    pev = e1 - e0,
    axis.x = axis.x,
    axis.y = axis.y,
    below0 = below0,
    above0 = above0,
    interpol = interpol
  )
}


