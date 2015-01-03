#' @title Visualizes the TPR and FPR on the RBP curve.
#'
#' @description For a given threshold \code{tresh}, the true positive rate (TPR) and the false positive
#' rate (FPR) can be visually assessed by the RBP curve by the intersection of the RBP curve with
#' the horizontal lines at \code{-thresh} and \code{1 - thresh}, respectively.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @param thresh [\code{numeric(1)}]\cr
#'   Threshold that is used to compute the true positve and false positive rate.
#'   Default is prevalence.
#' @export
addRates = function(obj, plot.values = TRUE, thresh = obj$prevalence, col = "grey") {

  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertNumber(thresh, lower = 0, upper = 1)

  # compute TPR and FPR for given threshold "t"
  tpr = mean(obj$pred[obj$y ==  1] > thresh)
  fpr = mean(obj$pred[obj$y ==  0] > thresh)
  prev = obj$prevalence
  one.minus.prev = obj$one.minus.prev

  # Add lines for TPR
  lines(x = c(par()$usr[1L], one.minus.prev+tpr*prev),
    y = c(1 - thresh, 1 - t),
    col = col, lty = 2L)
  lines(x = rep(one.minus.prev + tpr*prev, 2L),
    y = c(1 - thresh, par()$usr[4L]),
    col = col, lty = 2L)
  shape::Arrowhead(x0 = one.minus.prev + tpr*prev,
    y0 = par()$usr[4L], angle = 90L, arr.adj = 1L, arr.lwd = 1L,
    arr.length = 0.2, arr.col = col, lcol = col)

  # Add lines for FPR
  lines(x = c(fpr*one.minus.prev, fpr*one.minus.prev),
    y = c(-t, par()$usr[4L]),
    xpd = TRUE, col = col, lty = 2L)
  lines(x = c(par()$usr[1L], fpr*one.minus.prev),
    y = c(-t, -t),
    col = col, lty = 2L)
  shape::Arrowhead(x0 = fpr*one.minus.prev,
    y0 = par()$usr[4L], angle = 90L, arr.adj = 1L, arr.lwd = 1L,
    arr.length = 0.2, arr.col = col, lcol = col)

  # Add values for FPR and TPR into the plot
  if (plot.values) {
    text(x = (one.minus.prev + tpr*prev),
      y = par()$usr[4L],
      adj = c(1.1, 1), col = col,
      labels = bquote(paste(TPR(t), " = ", .(round(tpr, 3L)))))
    text(x = fpr*one.minus.prev,
      y = par()$usr[4L],
      xpd = TRUE, adj = c(0.5, 0), col = col,
      labels = bquote(paste(FPR(t), " = ", .(round(fpr, 3L)))))
    axis(2L, at = c(-t, 1 - t), labels = c("-t", "1 - t"), las = 2L,
      col.ticks = col, col.axis = col)
  }

  message("TPR(t = ", t, ") = ", round(tpr, digits = 4L))
  message("FPR(t = ", t, ") = ", round(fpr, digits = 4L))
  return(invisible(NULL))
}



