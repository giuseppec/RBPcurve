#' @title Visualizes the TPR and FPR on the RBP curve.
#'
#' @description For a given threshold \code{t}, the true positive rate (TPR) and the false positive 
#' rate (FPR) can be visually assessed by the RBP curve by the intersection of the RBP curve with 
#' the horizontal lines at \code{-t} and \code{1-t}, respectively.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @param t Threshold that is used to compute the true positve and false positive rate. 
#' @param ... currently not used
#' 
#' @import shape
#' @export

addRates = function(obj, plot.values = TRUE, 
  t = obj$prevalence, col = "grey", ...) {
  
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  # compute TPR and FPR for given threshold "t"
  tpr = mean(obj$pred[obj$y ==  1] > t)
  fpr = mean(obj$pred[obj$y ==  0] > t)
  prev = obj$prevalence
  oneMinusPrev = obj$oneMinusPrev

  # Add lines for TPR
  lines(x = c(par()$usr[1L], oneMinusPrev+tpr*prev), 
    y = c(1 - t, 1 - t), 
    col = col, lty = 2L)
  lines(x = rep(oneMinusPrev + tpr*prev, 2L),
    y = c(1 - t, par()$usr[4L]),  
    col = col, lty = 2L)
  Arrowhead(x0 = oneMinusPrev + tpr*prev, 
    y0 = par()$usr[4L], angle = 90L, arr.adj = 1L, arr.lwd = 1L,
    arr.length = 0.2, arr.col = col, lcol = col)
  
  # Add lines for FPR
  lines(x = c(fpr*oneMinusPrev, fpr*oneMinusPrev),
    y = c(-t, par()$usr[4L]), 
    xpd = TRUE, col = col, lty = 2L)
  lines(x = c(par()$usr[1L], fpr*oneMinusPrev),
    y = c(-t, -t), 
    col = col, lty = 2L)
  Arrowhead(x0 = fpr*oneMinusPrev, 
    y0 = par()$usr[4L], angle = 90L, arr.adj = 1L, arr.lwd = 1L, 
    arr.length = 0.2, arr.col = col, lcol = col)

  # Add values for FPR and TPR into the plot
  if (plot.values) {
    text(x = (oneMinusPrev + tpr*prev), 
      y = par()$usr[4L], 
      adj = c(1.1, 1), col = col,
      labels = bquote(paste(TPR(t), " = ", .(round(tpr, 3L)))))
    text(x = fpr*oneMinusPrev, 
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



