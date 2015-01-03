#' @title Visualizes a measure for good calibration on the RBP curve.
#'
#' @description The integral of the RBP curve is a measure for good calibration.
#' If the sum of the two integrals (below and above the RBP curve) is close to 0,
#' good calibration is satisfied and the prevalence is close to the average predicted probabilities.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param col [\code{vector(1)}]\cr
#'   Color for filling the polygon, as in \code{\link{polygon}}.
#'   Default is \dQuote{grey}.
#' @param border [\code{vector(1)}]\cr
#'   Color to draw the borders, as in \code{\link{polygon}}.
#'   Default is \code{NA} to omit borders.
#' @param ... [any]\cr
#'   Passed to \code{\link{polygon}}.
#' @template ret_invnull
#' @export
addGoodCalib = function(obj,
  plot.values = TRUE,
  col = "grey",
  border = NA,
  ...) {

  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertVector(col, len = 1L)
  assertVector(border, len = 1L)

  # Store values of obj
  x0 = obj$axis.x
  y1 = obj$axis.y

  # Highlights the integral below the RBP curve
  polygon(c(min(x0), x0, max(x0)), c(0, y1, 0), col = col, border = border, ...)

  # Should the values of the integral below and above the RBP curve be plotted into the current plot?
  if (plot.values) {
    text(x0[sum(obj$y == 0)], 0, adj = 1:0,
      labels = round(sum(obj$axis.y[obj$y == 0]) / obj$n, 4L))
    text(x0[sum(obj$y == 0)+1], 0, adj = 0:1,
      labels =round(sum(obj$axis.y[obj$y == 1]) / obj$n, 4L))
  }

  # Show message
  message("Integral below the RBP curve: ", round(sum(obj$axis.y[obj$y == 0]) / obj$n, 4L))
  message("Integral above the RBP curve: ", round(sum(obj$axis.y[obj$y == 1]) / obj$n, 4L))

  return(invisible(NULL))
}

