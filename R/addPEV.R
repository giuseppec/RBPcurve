#' @title Visualize the PEV on the RBP curve.
#'
#' @description
#' The PEV measure is the difference between the conditional expectation of the predicted
#' probabilities (conditional on the two groups that are determined by the target variable).
#' The PEV measure can be visually obtained by the RBP curve, namely by the difference of the
#' two areas that are Highlighted with \code{addPEV}.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param text.col [\code{character(1)} | \code{numeric(1)}]\cr
#'   Text color, used when \code{plot.values = TRUE}, otherwise ignored.
#'   Default is \dQuote{grey}.
#' @template arg_col
#' @export
addPEV = function(obj, plot.values = TRUE, text.col = "grey", col = "grey") {

  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assert(checkString(text.col), checkNumeric(text.col))
  assertVector(col, len = 1L)

  # Store values of obj
  x0 = obj$axis.x
  y1 = obj$axis.y
  one.minus.prev = obj$one.minus.prev

  # Highlight the area of the probabilities when Y=1
  polygon(x = c(x0[x0 > one.minus.prev], 1, x0[x0 > one.minus.prev][1L]),
    y = c(y1[x0 > one.minus.prev], 1, 1),
    border = NA, col = col)

  # Highlight the area of the probabilities when Y=0
  polygon(x = c(x0[1L], x0[x0 <= one.minus.prev], x0[length(x0[x0 <= one.minus.prev])]),
    y = c(0, y1[x0 <= one.minus.prev], 0),  border = NA, col = col)

  # Add values for E1 and E0 into the plot
  if (plot.values) {
    text(min(x0), 0, col = text.col, adj = 0:1,
      labels = bquote(paste(hat(E)[0], " = ", .(round(obj$e0, 4L)))))
    text(x0[length(x0[x0 <= one.minus.prev]) + 1], 1, adj = 0:1, col = text.col,
      labels = bquote(paste(hat(E)[1], " = ", .(round(obj$e1, 4L)))))
  }

  # Show message
  message("E1: Mean predicted probabilities for Y=1: ", round(obj$e1, 4L))
  message("E0: Mean predicted probabilities for Y=0: ", round(obj$e0, 4L))
  message("PEV = E1 - E0: ", round(obj$pev, 4L))
  return(invisible(NULL))
}

