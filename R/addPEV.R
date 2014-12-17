#' @title Highlights Area that is used to compute the PEV
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param ... passed to abline
#' @export
addPEV = function(obj, plot.values = TRUE, ...) {
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  x0 = obj$axis.x
  y1 = obj$axis.y

  oneMinusPrev = 1-obj$prevalence#x0[x0 > 1 - mean(obj$y)][1]

  polygon(c(x0[x0 > oneMinusPrev], 1, x0[x0 > oneMinusPrev][1]),
    c(y1[x0 > oneMinusPrev], 1, 1), border = NA,
    col = rgb(0, 0, 0, 0.25))

  polygon(c(x0[1], x0[x0<=oneMinusPrev], x0[length(x0[x0<=oneMinusPrev])]),
    c(0, y1[x0 <= oneMinusPrev], 0),  border = NA, col = rgb(0, 0, 0, 0.25))

  if (plot.values) {
    text(min(x0), 0, col = "gray30", adj = c(0,1),
      labels = bquote(paste(hat(E)[0], " = ", .(round(obj$e0, 4)))))

    text(x0[length(x0[x0<=oneMinusPrev])+1], 1, adj = c(0,1), col = "gray30",
      labels = bquote(paste(hat(E)[1], " = ", .(round(obj$e1, 4)))))
  }

  return(invisible(NULL))
}

