#' Make legend for DNE3d plot
#'
#' plotting subfunction
#'
#' @param DNELabels Numeric values for the labels
#' @param scaled Logical indicating whether the values are scaled
#' @param logColors Logical indicating colors are on log scale
#' @param size Numeric for legend scaling factor
#' @param signColor Logical indicating use of signed DNE values
#'
#' @details This is an internal function which builds a DNE plot legend
#'
#' The legend will reflect the elements used in the plot. This is an internal
#' function. Users will have little need to call or interact with it.
#' DNE_Legend()
#' 
#' @noRd

DNE_Legend <- function(DNELabels, scaled = F, logColors, size=1, signColor = TRUE){
  par(ann=F, mar=c(0,0,0,0))
  layout(matrix(1:2,ncol=2), widths = c(0.75, 0.25))
  plot(0,0, type='n', axes=F)
  lineSize <- 2 * size
  textSize <- 1.75 * size
  rectSize <- size
  
  x <- seq(0,1,l=100)
  if(logColors==TRUE){
    if(signColor == TRUE){
      colorslist <- signedcolor.gradient(rev(x))
    }
    if(signColor == FALSE){
      colorslist <- ccolor.gradient(rev(x))
    }
  }
  if(logColors==FALSE){
    if(signColor==T) {
      colorslist <- signedcolor.gradient(rev(x))
    }
    if(signColor==F) {
      colorslist <- ccolor.gradient(rev(x))
    }
  }
  legend_gradient <- as.raster(matrix(colorslist, ncol=1))
  
  XPos1 <- 0.8                      #X location of center (center of title, left edge legend text)
  XPos2 <- XPos1-(0.55*rectSize)    #X location of left edge rectangle
  XPos3 <- XPos1-(0.05*rectSize)    #X location of right edge rectangle
  XPos4 <- XPos1-(0.45*rectSize)    #X location of inside of left tic marks
  XPos5 <- XPos1-(0.15*rectSize)    #X location of inside of right tic marks
  YPos0 <- 0.5                      #Y location of center
  YPos1 <- YPos0+(0.3*rectSize)     #Y location of top of rectangle / legend text
  YPos2 <- YPos0-(0.25*rectSize)    #Y location of bottom of rectangle / legend text
  YPos3 <- YPos0+(0.4*rectSize)     #Y location of title
  YPos4 <- YPos0-(0.27*rectSize)    #Y location of top of box below rectangle
  YPosVex <- YPos0+(0.33*rectSize)
  YPosCav <- YPos0-(0.28*rectSize)
  
  plot(c(0,2),c(0,1), type = 'n', axes = F,xlab = '', ylab = '')
  PlotLabels <- round(DNELabels, digits=5)
  PlotLabels <- format(PlotLabels, scientific=FALSE)
  text(x=XPos1, y = seq(YPos2,YPos1,l=10), labels = PlotLabels, adj=c(0, NA), cex=textSize)
  rasterImage(legend_gradient, XPos2, YPos2, XPos3,YPos1)
  rect(XPos2, YPos2, XPos3, YPos1, lwd=lineSize)
  segments(x0=rep(XPos2, 10), y0=seq(YPos2,YPos1,l=10), x1=rep(XPos4, 10), y1=seq(YPos2,YPos1,l=10), lwd=lineSize)
  segments(x0=rep(XPos5, 10), y0=seq(YPos2,YPos1,l=10), x1=rep(XPos3, 10), y1=seq(YPos2,YPos1,l=10), lwd=lineSize)
  
  if(scaled==FALSE && logColors==FALSE){
    text(x=XPos1, y=YPos3, labels=c("DNE Value\nPer Face"), cex=textSize)
    if(signColor==T) {
    	text(x=XPos1, y=YPosVex, labels=c('Convex'), cex=textSize)
    	text(x=XPos1, y=YPosCav, labels=c('Concave'), cex=textSize)
    }
  }
  if(scaled==TRUE && logColors==FALSE){
    text(x=XPos1, y=YPos3, labels=c("Scaled\nDNE Value\nPer Face"), cex=textSize)
    if(signColor==T) {
    	text(x=XPos1, y=YPosVex, labels=c('Convex'), cex=textSize)
    	text(x=XPos1, y=YPosCav, labels=c('Concave'), cex=textSize)
    }
  }
  if(scaled==FALSE && logColors==TRUE){
    text(x=XPos1, y=YPos3, labels=c("Log DNE Value\nPer Face"), cex=textSize, font=2)
    if(signColor==T){
      text(x=XPos1, y=YPosVex, labels=c('Convex'), cex=textSize)
      text(x=XPos1, y=YPosCav, labels=c('Concave'), cex=textSize)
    }
  }
  if(scaled==TRUE && logColors==TRUE){
    text(x=XPos1, y=YPos3, labels=c("Scaled\nLog DNE Value\nPer Face"), cex=textSize)
  }
}
