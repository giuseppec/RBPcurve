#' @title Visualizes the prevalence on the RBP curve.
#'
#' @description The prevalence is the proportion of a population having a specific condition.
#' In binary classification, the condition refers to whether the target variable has the value 
#' \code{1}, that is, whether the target variable corresponds to the positive class.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @param ... currently not used
#' 
#' @template ret_invnull
#' 
#' @import shape
#' @export
addPrevalence = function(obj, plot.values = TRUE, col = "grey", ...) {
  assertClass(obj, "RBPObj")

  # Compute 1-prevalence
  oneMinusPrev = obj$oneMinusPrev

  # Plot vertical lines where the distance between the lines reflects the prevalence
  abline(v = c(oneMinusPrev, 1), col = col)
  Arrows(x0 = oneMinusPrev, x1 = 1, arr.col = col, col = col, lcol = col,
         y0 = -1, y1 = -1, code = 3, arr.adj = 1)
  
  # Should the value of the prevalence be plotted into the current plot?
  if (plot.values) {
    text(1-(mean(obj$y)/2), -1, col = col,
         bquote(paste(hat(theta), " = ", .(obj$prevalence))), pos = 3)
  }
  
  # Print message
  message("Prevalence: ", obj$prevalence)
  
  return(invisible(NULL))
}

