#' @title Visualizes the prevalence on the RBP curve.
#'
#' @description The prevalence is the proportion of a population having a specific condition.
#' In binary classification, the condition refers to whether the target variable has the value
#' \code{1}, that is, whether the target variable corresponds to the positive class.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @template ret_invnull
#' @export
addPrevalence = function(obj, plot.values = TRUE, col = "grey") {
  assertClass(obj, "RBPObj")

  # Compute 1-prevalence
  one.minus.prev = obj$one.minus.prev

  # Plot vertical lines where the distance between the lines reflects the prevalence
  abline(v = c(one.minus.prev, 1), col = col)
  shape::Arrows(x0 = one.minus.prev, x1 = 1L, arr.col = col, col = col, lcol = col,
    y0 = -1L, y1 = -1L, code = 3L, arr.adj = 1L)

  # Should the value of the prevalence be plotted into the current plot?
  if (plot.values) {
    text(1 - (mean(obj$y) / 2), -1L, col = col,
      bquote(paste(hat(theta), " = ", .(obj$prevalence))), pos = 3L)
  }

  # Print message
  message("Prevalence: ", obj$prevalence)

  return(invisible(NULL))
}

