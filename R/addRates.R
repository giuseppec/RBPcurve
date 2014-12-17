#' @title Add tpr and fpr on RBP curve.
#'
#' @description The true positive rate (tpr) and the false positive rate (fpr) can be visually
#' assessed by the RBP curve
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' @param t Threshold for computing the true positve and false positive rate. 
#' Default is the prevalence.
#' @param ... currently not used
#' 
#' @import shape
#' @export

addRates = function(obj, plot.values = TRUE, 
  t = obj$prevalence, col="gray", ...) {
  
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  # compute TPR and FPR for given threshold "t"
  tpr = mean(obj$pred[obj$y == 1] > t)
  fpr = mean(obj$pred[obj$y == 0] > t)
  prev = obj$prevalence
  oneMinusPrev = obj$oneMinusPrev

  # Add lines for TPR
  lines(x = c(par()$usr[1],oneMinusPrev+tpr*prev), 
        y = c(1-t, 1-t), 
        col = col, lty = 2)
  lines(x = rep(oneMinusPrev+tpr*prev, 2),
        y = c(1-t, par()$usr[4]),  
        col = col, lty = 2)
  Arrowhead(x0 = oneMinusPrev+tpr*prev, 
            y0 = par()$usr[4], angle = 90, arr.adj = 1, arr.lwd = 1,
            arr.length = 0.2, arr.col = col, lcol = col)
  
  # Add lines for FPR
  lines(x=c(fpr*oneMinusPrev, fpr*oneMinusPrev),
        y=c(-t, par()$usr[4]), 
        xpd=TRUE, col=col, lty=2)
  lines(x=c(par()$usr[1], fpr*oneMinusPrev),
        y=c(-t,-t), 
        col=col, lty=2)
  Arrowhead(x0=fpr*oneMinusPrev, 
            y0=par()$usr[4], angle = 90, arr.adj = 1, arr.lwd = 1, 
            arr.length=0.2, arr.col=col, lcol=col)

  # Display values for FPR and TPR
  if (plot.values) {
    text(x=(oneMinusPrev+tpr*prev), 
         y=par()$usr[4], 
         adj=c(1.1,1), col=col,
         labels=bquote(paste(TPR(t), "=",.(round(tpr, 3)))))
    text(x=fpr*oneMinusPrev, 
         y=par()$usr[4], 
         xpd=TRUE, adj=c(0.5,0), col=col,
         labels=bquote(paste(FPR(t), "=",.(round(fpr, 3)))))
    axis(2, at=c(-t, 1-t), labels = c("-t", "1-t"), las=2, col.ticks=col, col.axis=col)
  }

  message("TPR(t=",t,") = ", round(tpr, digits=4))
  message("FPR(t=",t,") = ", round(fpr, digits=4))
  return(invisible(NULL))
}



