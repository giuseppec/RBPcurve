#' @title Add tpr and fpr on RBP curve.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param ... currently not used
#' @export
# TODO: for different thresholds "t"
addRates = function(obj, plot.values = TRUE, ...) {
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  prev = obj$prev
  tpr = obj$tpr
  fpr = obj$fpr
  ymean = obj$ymean

  lines(x = rep((prev) + tpr * obj$ymean, 2),
    y = c(prev, 2),  col = "black", lty = 2)
  lines(x = c(-1, prev + tpr * ymean),
    y = c(prev, prev), col = "black", lty = 2)
  Arrowhead(prev + tpr*ymean,
    par()$usr[4], angle = 90, arr.adj = 1, arr.lwd = 1,
    arr.length = 0.2, arr.col = "black", lcol = "black")
  text(x = ((prev)+tpr*ymean),
    y = par()$usr[4], adj = c(1.1,1), #prev,
    labels = bquote(paste(tpr(t), " = ",.(round(tpr, 3)))),  col = "black")

  lines(x = c(fpr * prev, fpr * prev),
    y = c(-ymean, par()$usr[3]),  xpd = TRUE, col = "black", lty = 2)
  lines(x = c(-1, fpr*(prev)), y = c(-ymean,-ymean), col = "black", lty = 2)
  text(x = fpr*(prev), y = par()$usr[3],
    labels = bquote(paste(fpr(t), " = ",.(round(fpr, 3)))), adj = c(-0.1,0), col = "black")
  Arrowhead(fpr*(prev), arr.adj = 1, arr.lwd = 1,
    par()$usr[3], angle = 270,
    arr.length = 0.2, arr.col = "black", lcol = "black")

  return(invisible(NULL))
}



