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

  prev = x0[x0 > 1 - mean(y)][1]

  abline(v = prev, col = "gray50")
  abline(h = 0, col = "gray50")

  #   mtext(expression(paste(hat(F)[epsilon], "(", hat(epsilon), "|D = 0)")),
  #        side = 1, line = 2,  at = (prev)/2, col = "gray50", cex = allcex)
  #   mtext(expression(paste(hat(F)[epsilon], "(", hat(epsilon), "|D = 1)")),
  #        side = 3, line = 1.5,  at = (prev)+(mean(y))/2, col = "gray50", cex = allcex)

  polygon(c(x0[x0 >= prev], x0[x0 >= prev][1]),
    c(c(y1[x0 >= prev][-length(y1[x0 >= prev])],1), 1), border = NA,
    col = rgb(0, 0, 0, 0.25))

  polygon(c(x0[1], x0[x0<prev], x0[length(x0[x0<prev]) + 1]),
    c(0, y1[x0 < prev], 0),  border = NA, col = rgb(0, 0, 0, 0.25))

  if (plot.values) {
    text(min(x0), 0, col = "gray30", adj = c(0,1),
      labels = bquote(paste(hat(E)[0], " = ", .(round(obj$e0, 4)))))

    text(x0[length(x0[x0<prev])+1], 1, adj = c(0,1), col = "gray30",
      labels = bquote(paste(hat(E)[1], " = ", .(round(obj$e1, 4)))))
  }

  return(invisible(NULL))
}

