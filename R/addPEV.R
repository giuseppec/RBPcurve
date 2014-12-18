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
#' @template arg_text.col
#' @template arg_col
#' @param ... currently not used
#' @export
addPEV = function(obj, 
  plot.values = TRUE, 
  text.col = NULL, 
  col = "grey", 
  ...) {
  
  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertVector(col, len=1)

  # Store values of obj
  x0 = obj$axis.x
  y1 = obj$axis.y
  oneMinusPrev = obj$oneMinusPrev 

  # Highlight the area of the probabilities when Y=1
  polygon(c(x0[x0 > oneMinusPrev], 1, x0[x0 > oneMinusPrev][1]),
    c(y1[x0 > oneMinusPrev], 1, 1), border = NA,
    col = col)

  # Highlight the area of the probabilities when Y=0
  polygon(x = c(x0[1], x0[x0<=oneMinusPrev], x0[length(x0[x0<=oneMinusPrev])]),
    y = c(0, y1[x0 <= oneMinusPrev], 0),  border = NA, col = col)

  # Add values for E1 and E0 into the plot
  if (plot.values) {
    if (is.null(text.col)) text.col = "grey"
    text(min(x0), 0, col = text.col, adj = c(0,1),
      labels = bquote(paste(hat(E)[0], " = ", .(round(obj$e0, 4)))))

    text(x0[length(x0[x0<=oneMinusPrev])+1], 1, adj = c(0,1), col = text.col,
      labels = bquote(paste(hat(E)[1], " = ", .(round(obj$e1, 4)))))
  } else {
    if(!is.null(text.col)) assertNull(text.col)
  }
  
  # Show message
  message("E1: Mean predicted probabilities for Y=1: ", round(obj$e1,4))
  message("E0: Mean predicted probabilities for Y=0: ", round(obj$e0,4))
  message("PEV = E1 - E0: ", round(obj$pev,4))
  return(invisible(NULL))
}

