#' @title Plot residual-based predictiveness curve.
#'
#' @template arg_obj
#' @param main plot title
#' @param xlab
#' @param ylab label for y-axis
#' @param type plot type
#' @param ylim limits for y-axis
#' @param xlab [\code{character(1)}]\cr
#'  Label for X-axis.
#'  Default is \dQuote{Cumulative Percentage}.
#' @param ylab [\code{character(1)}]\cr
#'  Label for Y-axis.
#'  Default is \dQuote{Estimated Residuals}.
#' @param type [\code{logical(1)}]\cr
#'  Should an additional axis be plotted reflecting residuals conditional on y?
#'  Default is \code{FALSE}.
#' @param cond.axis [\code{logical(1)}]\cr
#'  Should an additional axis be plotted reflecting residuals conditional on y?
#'  Default is \code{FALSE}.
#' @param add [\code{logical(1)}]\cr
#'  Should RBP plot be added to current plot?
#'  Default is \code{FALSE}.
#' @param title.line where to plot the title
#' @param ... passed to plot or lines (depending on add)
#' @export
plotRBPcurve = function (obj,
  main = "RBP Curve",
  xlab = "Cumulative Percentage",
  ylab = "Estimated Residuals",
  type = "l",
  ylim = c(-1,1),
  conditionalAxis = FALSE,
  title.line = ifelse(conditionalAxis, 3, 2),
  add = FALSE,
  ...) {

  assertClass(obj, "RBPObj")
  assertString(main)
  assertString(xlab)
  assertString(ylab)
  assertFlag(add)

  if (add) {
    lines(x = obj$axis.x, y = obj$axis.y, ...)
  } else {
    plot(x = obj$axis.x, y = obj$axis.y,
      xlab = xlab, ylab = ylab, ylim = ylim,
      main = "", type = type, ...)
    abline(h = 0, col = "gray")
  }

  interpol = obj$interpol
  prev1m = interpol$x[which.min(abs(interpol$y))]
  xAxis = seq(0,1, by = 0.2)

  if (conditionalAxis) {
    axis(side = 3, at = xAxis*prev1m, labels = xAxis, col.axis = "gray50",
      padj = c(rep(NA, length(xAxis)-1), 0.5),
      col = "gray50")
    axis(side = 3, at = prev1m+xAxis*(1-prev1m), col.axis = "gray50", labels = xAxis,
      padj = c(-0.5,rep(NA, length(xAxis)-1)),
      col = "gray50")
  }

  title(main, line = title.line)
  invisible(NULL)

}

