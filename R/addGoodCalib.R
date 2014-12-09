#' @title Displays integrals that refer to good calibration.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template ret_invnull
#' @export
addGoodCalib = function(obj, plot.values = TRUE) {
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  x0 = obj$axis.x
  y1 = obj$axis.y

  polygon(c(min(x0), x0, max(x0)), c(0, y1, 0),  border = NA,
    col = rgb(0, 0, 0, 0.25))

  if (plot.values) {
    text(x0[1], 0, adj = c(0,0), #pos = 3,
      labels = round(sum(obj$axis.y[obj$y == 0]) / obj$n, 4))
    text(x0[length(x0)], 0, adj = c(1,1),
      labels =round(sum(obj$axis.y[obj$y == 1]) / obj$n, 4))
  }
  invisible(NULL)
}

