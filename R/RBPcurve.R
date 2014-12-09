##' Function plots RBP curve
##'
##' The function is an improved version of \code{\link[PredictABEL]{plotPredictivenessCurve}} and 
##' creates a plot of cumulative percentage of individuals to the predicted risks. 
##' See \code{\link[PredictABEL]{plotPredictivenessCurve}} for details on usage and arguments.
##'

computeAxis <- function(pred, y){
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  eps <- y - pred
  yAxis <- sort(eps)
  n <- length(eps)
  x <- (1:n)/n
  return(list("x"=x, "y"=yAxis))
}

RBPcurve <- function (pred, y, 
                      main="RBP Curve", 
                      xlab="Cumulative Percentage", 
                      ylab="Estimated Residuals", 
                      type="l",
                      ylim=c(-1,1), 
                      conditionalAxis=FALSE,
                      title.line=ifelse(conditionalAxis, 3, 2),
                      add=FALSE,
                      ...) 
{
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  xyAxis <- computeAxis(pred, y)
  
  if (add) {
    lines(x = xyAxis$x, y = xyAxis$y, ...)
  } else {
    plot(x = xyAxis$x, y = xyAxis$y, 
         xlab = xlab, ylab = ylab, ylim=ylim,
         main = "", type=type, ...)
    abline(h=0, col="gray")
  }
  
  below0 <- max(which(xyAxis$y<0))
  above0 <- min(which(xyAxis$y>0))
  interpol <- approx(xyAxis$x[c(below0,above0)], xyAxis$y[c(below0,above0)], n=1000)
  prev1m <- interpol$x[which.min(abs(interpol$y))]
  
  xAxis <- seq(0,1, by=0.2)
  
  if (conditionalAxis) {
    axis(side=3, at=xAxis*(prev1m), labels=xAxis, col.axis="gray50", 
         padj=c(rep(NA, length(xAxis)-1),0.5),
         col="gray50")
    axis(side=3, at=prev1m+xAxis*(1-prev1m), col.axis="gray50", labels=xAxis, 
         padj=c(-0.5,rep(NA, length(xAxis)-1)), 
         col="gray50")
  }
  
  title(main, line=title.line)
  invisible(as.data.frame(xyAxis))
}

addPrevalence <- function(pred, y, col="gray", value=TRUE, 
                          ...){
  xyAxis <- computeAxis(pred, y)
  
  below0 <- max(which(xyAxis$y<0))
  above0 <- min(which(xyAxis$y>0))
  
  interpol <- approx(xyAxis$x[c(below0,above0)], xyAxis$y[c(below0,above0)], n=1000)
  
  # interpolated 1-pervalence for plotting purposes
  prev1m <- interpol$x[which.min(abs(interpol$y))]
  
  abline(v=prev1m, col=col, ...)
  if(value)
    axis(1, at=prev1m, 
         label=bquote(paste(hat(theta),"=", .(round(mean(pred),4)))), 
         padj=-3, hadj=0, col=col, col.axis=col)
  return(mean(pred))
}

addPEV <- function(pred, y, xAxis = seq(0,1, by=0.2), ...){
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  xyAxis <- computeAxis(pred, y)
  x0 <- xyAxis$x
  y1 <- xyAxis$y
  
  prev <- x0[x0>1-mean(y)][1]
  
  abline(v=prev, col="gray50")
  abline(h=0, col="gray50")

  #mtext(expression(paste(hat(F)[epsilon], "(", hat(epsilon), "|D=0)")), 
  #      side = 1, line = 2,  at=(prev)/2, col="gray50", cex=allcex)
  #mtext(expression(paste(hat(F)[epsilon], "(", hat(epsilon), "|D=1)")), 
  #      side = 3, line = 1.5,  at=(prev)+(mean(y))/2, col="gray50", cex=allcex)
  
  polygon(c(x0[x0>=prev], x0[x0>=prev][1]), 
          c(c(y1[x0>=prev][-length(y1[x0>=prev])],1), 1),  border=NA,
          col = rgb(0, 0, 0, 0.25)) 

  
  polygon(c(x0[1], x0[x0<prev], x0[length(x0[x0<prev])+1]), 
          c(0, y1[x0<prev], 0),  border=NA, col = rgb(0, 0, 0, 0.25)) 
  
  E1 <- mean(pred[y==1])
  E0 <- mean(pred[y==0])
#   text(x0[length(x0[x0<prev])+1], 0.5, labels=bquote(paste(hat(E)[1], "=", .(E1))), pos=4, col="gray30")
#   text(0.01, 0, labels=bquote(paste(hat(E)[0], "=", .(E0))), col="gray30", adj=c(0,1))
  return(list(E0=E0, E1=E1, PEV=E1-E0))
}

# TODO: for different thresholds "t"
addRates <- function(pred, y, ...){
  require(shape)
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  xyAxis <- computeAxis(pred, y)
  
  prev <- 1-mean(y)
  TPR<-mean(pred[y==1]>mean(y))
  lines(x=rep((prev)+TPR*mean(y), 2),
        y=c(prev, 2),  col="black", lty=2)
  lines(x=c(-1,(prev)+TPR*mean(y)), 
        y=c(prev,prev), col="black", lty=2)
  Arrowhead((prev)+TPR*mean(y), 
            par()$usr[4], angle = 90, arr.adj = 1, arr.lwd = 1,
            arr.length=0.2, arr.col="black", lcol="black")
  text(x=((prev)+TPR*mean(y)), 
       y=par()$usr[4], adj=c(1.1,1), #prev, 
       labels=bquote(paste(TPR(t), "=",.(round(TPR, 3)))),  col="black")
  
  FPR <-(mean(pred[y==0]>mean(y)))
  lines(x=c(FPR*(prev), FPR*(prev)),
        y=c(-mean(y), par()$usr[3]),  xpd=TRUE, col="black", lty=2)
  lines(x=c(-1, FPR*(prev)), y=c(-mean(y),-mean(y)), col="black", lty=2)
  text(x=FPR*(prev), y=par()$usr[3],
       labels=bquote(paste(FPR(t), "=",.(round(FPR, 3)))), adj=c(-0.1,0), col="black")
  Arrowhead(FPR*(prev), arr.adj = 1, arr.lwd = 1,
            par()$usr[3], angle = 270, 
            arr.length=0.2, arr.col="black", lcol="black")
  
}

addGoodCalib <- function(pred, y, value=TRUE, ...){
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  xyAxis <- computeAxis(pred, y)
  x0 <- xyAxis$x
  y1 <- xyAxis$y
  
  polygon(c(min(x0), x0, max(x0)), c(0, y1, 0),  border=NA,
          col = rgb(0, 0, 0, 0.25)) 
  
  if(value){
    text(x0[1], 0,  adj = c(0,0),#pos=3,
         labels=round(sum(xyAxis$y[y==0])/length(xyAxis$y), 4))
    text(x0[length(x0)], 0, adj = c(1,1),
         labels =round(sum(xyAxis$y[y==1])/length(xyAxis$y), 4))
  }
  return(list("Y=0"=sum(xyAxis$y[y==0])/length(xyAxis$y), 
              "Y=1"=sum(xyAxis$y[y==1])/length(xyAxis$y)))
}

addWellCalib <- function(pred, y, ...){
  if (is.factor(y)) {
    y <- as.numeric(y)-1
  }
  xyAxis <- computeAxis(pred, y)
  
  quant <- seq(0,1, by=0.1)
  up <- 1-quant
  lo <- -quant
  colSeq <- seq(0.3,  0.95, length=length(quant))
  color <- rgb(0, 0, 0, colSeq) #rgb(quant^2, quant, (1-quant)^2, colSeq)
  x0 <-seq(0,1, length=length(pred))
  
  y1 <- xyAxis$y
  areas <- matrix(ncol=2, nrow=(length(quant)-1))
  
  ind <- NA
  pos <- numeric(length(quant)-1)
  neg <- numeric(length(quant)-1)
  for (i in 1:(length(quant)-1)) {
    diff <- (y-pred)[pred<quant[i+1] & pred>=quant[i]]
    diff1 <- (y-pred)[pred<quant[i+1] & pred>=quant[i] & y==1]
    diff0 <- (y-pred)[pred<quant[i+1] & pred>=quant[i] & y==0]
    if (length(diff1)<2 | length(diff0)<2) {
      if(any(is.na(ind))) ind <- i else ind <- c(ind, i)
      #message("fewer than two observations between the qantiles [", quant[i], ", ", quant[i+1],"]")
    } 
    if (length(diff1) != 0 & length(diff0) != 0){
      section <- x0[y1<up[i] & y1>=up[i+1]]
      polygon(c(min(section), section, max(section)), 
              c(0, y1[y1<up[i] & y1>=up[i+1]], 0),  
              border=1, col = color[i]) 
      pos[i] <- round(sum(diff1)/length(y1), 4)
      
      areas[i, 1] <- sum(diff1)/length(y1)
      
      section <- x0[y1 < lo[i] & y1 >= lo[i+1] ]
      polygon(c(min(section), section, max(section)), 
              c(0, y1[y1<lo[i] & y1 >= lo[i+1]], 0),  
              border=1, col = color[i]) 
      neg[i] <- round(sum(diff0)/length(y1), 4)
      
      areas[i, 2] <- sum(diff0)/length(y1)
    }
    
  }
  quantiles <- gsub(" ", ", ", do.call("paste", data.frame(quant[1:10], quant[2:11])))

  message("fewer than two observations in qantile: ", 
          paste("[", quantiles[ind],"]", sep="", collapse=", "))
  
  subplot(fun={barplot(rbind(pos,abs(neg)), beside=T, col=rep(color, each=2), 
                       cex.axis=1, main="Area", las=2)}, 
          x=c(0.125,0.5),
          y=c(0.4,0.9),
          pars=list(mar=c(0,0,1,0)+0.1) )
  
  row.names(areas) <- paste("[", quantiles,"]", sep="")
  colnames(areas) <- c("Y=1", "Y=0")
  return(areas)
}