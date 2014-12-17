#' @title Add TPR and FPR on RBP curve.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @template arg_col
#' 
#' @return A matrix.
#' 
#' @import TeachingDemos
#' @export
addWellCalib = function(obj, plot.values = TRUE, col=rgb(0, 0, 0, seq(0.1,  0.9, length = 10))) {
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)

  pred = obj$pred
  y = obj$y
  n = obj$n

  
  quant = seq(0,1, by = 0.1)
  up = 1 - quant
  lo = -quant
  #colSeq = seq(0.3,  0.95, length = length(quant))
  color = col#rgb(0, 0, 0, colSeq) 
  x0 = obj$axis.x 

  y1 = obj$axis.y
  areas = matrix(ncol = 2, nrow = (length(quant)-1))

  ind = NA
  pos = numeric(length(quant)-1)
  neg = numeric(length(quant)-1)
  for (i in 1:(length(quant)-1)) {
    diff = (y-pred)[pred<quant[i+1] & pred >= quant[i]]
    diff1 = (y-pred)[pred<quant[i+1] & pred >= quant[i] & y ==1]
    diff0 = (y-pred)[pred<quant[i+1] & pred >= quant[i] & y ==0]
    if (length(diff1) < 2L | length(diff0) < 2L) {
      if (any(is.na(ind))) ind = i else ind = c(ind, i)
      #message("fewer than two observations between the qantiles [", quant[i], ", ", quant[i+1],"]")
    }
    if (length(diff1) != 0) {
      section = x0[y1 < up[i] & y1 >= up[i+1]]
      polygon(c(min(section), section, max(section)),
        c(0, y1[y1<up[i] & y1 >= up[i+1]], 0),
        border = 1, col = color[i])
      pos[i] = round(sum(diff1)/n, 4)

      areas[i, 1] = sum(diff1)/n
    }

    if (length(diff0) != 0) {
      section = x0[y1 < lo[i] & y1 >= lo[i+1] ]
      polygon(c(min(section), section, max(section)),
        c(0, y1[y1<lo[i] & y1 >= lo[i+1]], 0),
        border = 1, col = color[i])
      neg[i] = round(sum(diff0) / n, 4)

      areas[i, 2] = sum(diff0) / n
    }

  }
  quantiles = gsub(" ", ", ", do.call("paste", data.frame(quant[1:10], quant[2:11])))

  message("fewer than two observations with probabilities between: ",
          paste("[", quantiles[ind],"]", sep = "", collapse = ", "))

  # 
  if (plot.values) {
    subplot(fun = {
      barplot(rbind(pos,abs(neg)), beside = T, col = rep(color, each = 2),
        cex.axis = 1, main = "Area", las = 2)
      },
      x = c(0.125,0.5),
      y = c(0.4,0.9),
      pars = list(mar = c(0,0,1,0)+0.1)
    )
  }

  row.names(areas) = paste("[", quantiles,"]", sep = "")
  colnames(areas) = c("Y = 1", "Y = 0")
  return(invisible(areas))
}

