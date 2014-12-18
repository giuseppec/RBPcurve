#' @title Visualizes a measure for well calibration on the RBP curve.
#' 
#' @description A measure for a well calibrated model can be obtained by grouping the predicted 
#' probabilities via deciles and comparing the mean of the predicted probabilities within each 
#' decile in the two groups that are determined by the target variable.
#'
#' @template arg_obj
#' @template arg_plotvalues
#' @param col [\code{character(10)} | \code{numeric(10)} ]\cr
#' A specification for the the plotting color for the areas. 
#' @param ... currently not used
#' 
#' @return A matrix that contains the average of the "probabilities within deciles" conditional on Y.
#' 
#' @import TeachingDemos
#' @import shape
#' @export
addWellCalib = function(obj, 
  plot.values = TRUE, 
  col = greycol(10L),
  ...) {
  
  # Check arguments
  assertClass(obj, "RBPObj")
  assertFlag(plot.values)
  assertVector(col, min.len = 1L, max.len = 10L)
  
  # Store values of obj
  pred = obj$pred
  y = obj$y
  n = obj$n
  x0 = obj$axis.x
  y1 = obj$axis.y
  
  # Set range for deciles
  quant = seq(0, 1, by = 0.1)
  up = 1 - quant
  lo = -quant
  quantiles = gsub(" ", ", ", do.call("paste", data.frame(quant[1:10], quant[2:11])))
  
  # Matrix that will contain the average of the "probabilities within deciles" conditional on Y.
  areas = matrix(ncol = 2, nrow = (length(quant) - 1))
  row.names(areas) = paste("[", quantiles,"]", sep = "")
  colnames(areas) = c("Y = 1", "Y = 0")

  # Preallocation
  ind = NA
  pos = numeric(length(quant) - 1)
  neg = numeric(length(quant) - 1)
  
  for (i in 1:(length(quant) - 1)) {
    # Residuals for different conditions
    diff = (y - pred)[pred < quant[i + 1L] & pred >= quant[i]]
    diff1 = (y - pred)[pred < quant[i + 1L] & pred >= quant[i] & y == 1]
    diff0 = (y - pred)[pred < quant[i + 1L] & pred >= quant[i] & y == 0]
    
    # Store deciles where the number of observations is smaller than 2
    if (length(diff1) < 2L | length(diff0) < 2L) {
      if (any(is.na(ind))) ind = i else ind = c(ind, i)
    }
    
    # Highlight the area for the probabilities of the i-th quantile when Y=1
    if (length(diff1) != 0L) {
      section = x0[y1 < up[i] & y1 >= up[i + 1L]]
      polygon(x = c(min(section), section, max(section)),
        y = c(0, y1[y1 < up[i] & y1 >= up[i + 1L]], 0),
        border = 1L, col = col[i])
      pos[i] = round(sum(diff1) / n, 4L)

      areas[i, 1L] = sum(diff1) / n
    }

    # Highlight the area for the probabilities of the i-th quantile when Y=0
    if (length(diff0) != 0L) {
      section = x0[y1 < lo[i] & y1 >= lo[i + 1L] ]
      polygon(c(min(section), section, max(section)),
        c(0, y1[y1 < lo[i] & y1 >= lo[i + 1L]], 0),
        border = 1L, col = col[i])
      neg[i] = round(sum(diff0) / n, 4L)

      areas[i, 2L] = sum(diff0) / n
    }
  }

  # Add values for E1 and E0 into the plot
  if (plot.values) {
    subplot(fun = {
      barplot(rbind(pos, abs(neg)), beside = TRUE, col = rep(col, each = 2L),
        cex.axis = 1L, main = "Area", las = 2L)
      },
      x = c(0.125, 0.5),
      y = c(0.4 ,0.9),
      pars = list(mar = c(0, 0, 1, 0) + 0.1)
    )
  }

  # Show message
  message("fewer than two observations with probabilities between: ",
    paste("[", quantiles[ind],"]", sep = "", collapse = ", "))
  
  return(invisible(areas))
}

