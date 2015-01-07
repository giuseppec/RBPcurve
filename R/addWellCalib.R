#' @title Visualizes a measure for well calibration on the RBP curve.
#'
#' @description A measure for a well calibrated model can be obtained by
#' grouping the predicted probabilities via deciles yielding 10 groups.
#' The equally collored areas belong to a specific group. When each of the two  
#' equally collored areas are similar, the model is well calibrated.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param col [\code{character} | \code{numeric}]\cr
#'   A specification for the the plotting color for the areas.
#' @param pos [\code{character(1)}] 
#'   Determines the position of the subplot that is plotted when \code{plot.values = TRUE}. 
#'   Can be either \code{pos = "topleft"} or \code{pos = "bottomright"}.
#'   Default is \code{pos = "topleft"}.
#' @return A matrix that contains the average of the \dQuote{probabilities within deciles}
#'   conditional on Y.
#' @export
#' 
addWellCalib = function(obj, plot.values = TRUE, 
  col = shape::greycol(10L), pos = "topleft") {

  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertVector(col, min.len = 1L, max.len = 10L)
  assertChoice(pos, c("topleft", "bottomright"))
  
  # Store values of obj
  pred = obj$pred
  y = obj$y
  n = obj$n
  x1 = obj$axis.x
  y1 = obj$axis.y
  eps = y - pred
  
  # Set range for deciles
  q = seq(0, 1, by = 0.1)
  up = 1 - q
  lo = -q

  # Matrix that will contain the average of the "probabilities within deciles" conditional on Y.
  areas = matrix(ncol = 2L, nrow = (length(q) - 1))
  row.names(areas) = sprintf("[%s, %s]", q[1:10], q[2:11])
  colnames(areas) = c("Y = 0", "Y = 1")
  
  for (i in 1:(length(q) - 1)) {
    # Residuals for different conditions
    eps1 = eps[pred < q[i + 1L] & pred >= q[i] & y == 1L]
    eps0 = eps[pred < q[i + 1L] & pred >= q[i] & y == 0L]
    
    # Highlight the area for the probabilities bounded by the i-th and (i+1)-th decile
    if (length(eps0) != 0L) areaCalib(x1, y1, lo[i], col[i])
    if (length(eps1) != 0L) areaCalib(x1, y1, up[i], col[i])
    
    areas[i, ] = c(sum(eps0), sum(eps1)) / n
  }
  
  # Add values for E1 and E0 into the plot
  if (plot.values) {
    if (pos == "topleft") {
      x = c(0.1, 1 - obj$prev)
      y = c(0.3, 0.8) 
    } else {
      x = c(1.1 - obj$prev, 1)
      y = c(-0.8, -0.3) 
    }
    
    TeachingDemos::subplot(fun = {
      barplot(t(abs(areas)), beside = TRUE, col = rep(col, each = 2L),
        main = "Area", las = 2L, xaxt = "n")
      }, x = x, y = y, pars = list(mar = c(0, 0, 1, 0) + 0.1)
    )
  }
  
  return(invisible(areas))
}

# helper for highlighting the area bounded by deciles of predicted risks
areaCalib = function(x1, y1, thres, col) {
  ind = (y1 < thres) & (y1 >= thres - 0.1)
  if (sum(ind) != 0L) {
    polygon(x = c(min(x1[ind]), x1[ind], max(x1[ind])),
      y = c(0, y1[ind], 0), border = 1L, col = col)
  }
}
