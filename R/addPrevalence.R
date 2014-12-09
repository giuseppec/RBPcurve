#' @title Add prevalence to RBP curve.
#'
#' @template arg_obj
#' @param col color
#' @template arg_plotvalues
#' @param ... passed to abline
#' @template ret_invnull
#' @export
addPrevalence = function(obj, col = "gray", plot.values = TRUE, ...) {
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  # interpolated 1-pervalence for plotting purposes
  prev1m = obj$interpol$x[which.min(abs(obj$interpol$y))]

  abline(v = prev1m, col = col, ...)
  if (plot.values) {
    axis(1, at = prev1m, labels = bquote(paste(hat(theta)," = ", .(round(mean(obj$pred),4)))),
      padj = -3, hadj = 0, col = col, col.axis = col)
  }
}

