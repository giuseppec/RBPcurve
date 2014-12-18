#' @title Plot residual-based predictiveness (RBP) curve.
#'
#' @template arg_obj
#' @param main An overall title for the plot.
#' @param xlab [\code{character(1)}]\cr
#'  Label for X-axis.
#'  Default is \dQuote{Cumulative Percentage}.
#' @param ylab [\code{character(1)}]\cr
#'  Label for Y-axis.
#'  Default is \dQuote{Estimated Residuals}.
#' @param type [\code{character(1)}]\cr
#'  The plot type that should be drawn, see \code{\link{plot}} for all possible types. 
#'  Default is \code{type = "l"} for \bold{l}ines.
#' @param ylim limits for Y-axis. Default is \code{ylim = c(-1,1.1)}.
#' @param cond.axis [\code{logical(1)}]\cr
#'  Should an additional axis be plotted reflecting residuals conditional on y?
#'  Default is \code{FALSE}.
#' @param title.line where to plot the title, see \code{\link{title}}.
#' @param add [\code{logical(1)}]\cr
#'  Should RBP plot be added to current plot?
#'  Default is \code{FALSE}.
#' @param ... passed to plot or lines (depending on add)
#' @export
plotRBPcurve = function (obj,
  main = "RBP Curve",
  xlab = "Cumulative Percentage",
  ylab = "Estimated Residuals",
  type = "l",
  ylim = c(-1, 1.1),
  cond.axis = FALSE,
  title.line = ifelse(cond.axis, 3, 2),
  add = FALSE,
  ...) {

  # argument checks
  assertClass(obj, "RBPObj")
  assertString(main)
  assertString(xlab)
  assertString(ylab)
  assertFlag(add)

  # plot or add
  if (add) {
    lines(x = obj$axis.x, y = obj$axis.y, ...)
  } else {
    plot(x = obj$axis.x, y = obj$axis.y,
      xlab = xlab, ylab = ylab, ylim = ylim,
      main = "", type = type, yaxt="n", ...)
    axis(2, las=2)
    abline(h = 0, col = "grey")
  }

  #interpol = obj$interpol
  #prev1m = obj$vertical.line #interpol$x[which.min(abs(interpol$y))]
  oneMinusPrev = obj$oneMinusPrev
  xAxis = seq(0,1, by = 0.2)

  if (cond.axis) {
    abline(v = oneMinusPrev, col = "grey")
    axis(side = 1, at = xAxis*oneMinusPrev, labels = xAxis, 
         padj=-0.5, hadj=0.75, pos=par()$usr[4])
      
    axis(side = 3, at = oneMinusPrev+xAxis*(1-oneMinusPrev),
         padj=0.5, hadj=0.25, labels = xAxis)
  }

  title(main, line = title.line)
  
  return(invisible(NULL))
}

