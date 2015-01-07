#' @title Visualizes the TPR and FPR on the RBP curve.
#'
#' @description For a given threshold \code{tresh}, the true positive rate (TPR) 
#' and the false positive rate (FPR) can be visually assessed by the RBP curve 
#' by the intersection of the RBP curve with the horizontal lines at 
#' \code{-thresh} and \code{1 - thresh}, respectively.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @param thresh [\code{numeric(1)}]\cr
#'   Threshold that is used to compute the true positve and false positive rate.
#'   Default is prevalence.
#' @template ret_invnull
#' @export
addRates = function(obj, plot.values = TRUE, 
  thresh = obj$prev, col = "black") {

  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertNumber(thresh, lower = 0, upper = 1)

  # compute TPR and FPR for given threshold "t"
  tpr = mean(obj$pred[obj$y == 1] > thresh)
  fpr = mean(obj$pred[obj$y == 0] > thresh)
  prev = obj$prev
  omp = obj$one.min.prev

  # compute x-values on the RBP curve
  xtpr = omp + tpr*prev
  xfpr = fpr*omp
  
  # Add lines for TPR
  lines(x = c(par()$usr[1L], xtpr), y = rep(1 - thresh, 2),
    col = col, lty = 2L)
  lines(x = rep(xtpr, 2L), y = c(1 - thresh, par()$usr[4L]),
    col = col, lty = 2L)
  shape::Arrowhead(x0 = xtpr, y0 = par()$usr[4L], angle = 90L, 
    arr.adj = 1L, arr.lwd = 1L, arr.length = 0.2, 
    arr.col = col, lcol = col)

  # Add lines for FPR
  lines(x = rep(xfpr, 2), y = c(-thresh, par()$usr[4L]),
    xpd = TRUE, col = col, lty = 2L)
  lines(x = c(par()$usr[1L], xfpr), y = c(-thresh, -thresh),
    col = col, lty = 2L)
  shape::Arrowhead(x0 =xfpr, y0 = par()$usr[4L], angle = 90L, 
    arr.adj = 1L, arr.lwd = 1L, arr.length = 0.2, 
    arr.col = col, lcol = col)

  # Add values for FPR and TPR into the plot
  if (plot.values) {
    text(x = xtpr, y = par()$usr[4L], adj = c(1.1, 1), col = col,
      labels = bquote(paste(TPR(thresh), " = ", .(round(tpr, 3L)))))
    text(x = xfpr, y = par()$usr[4L], xpd = TRUE, pos=3, 
         col = col, labels = sprintf("FPR(thresh) = %s", round(fpr, 3L))
      )
  }

  return(invisible(NULL))
}



