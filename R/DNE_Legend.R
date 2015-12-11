#' Make legend for DNE3d plot
#'
#' plotting subfunction
#'
#' @param start value for the legend to start with, i.e. bottome value
#' @param end value for the legend to end with, i.e. top value
#' @param colors range of values, defaulting to heat colors
#' @param DNELabels values for the labels
#' @param scaled logical indicating whether the values are scaled
#' @param edgeMask logical indicating whether of not edges are being masked
#' and that information to be included in the legend
#' @param outlierMask logical indicating whether outliers are masked
#' @param logColors logical indicating colors are on log scale
#'
#' @details This is an internal function which builds a better DNE plot legend
#'
#' The legend will reflect the elements used in the plot. This is an internal
#' function. Users will have little need or call to interact with it.
#'
#'
#' @import
#' graphics
#'
#' @export
#' DNE_Legend

DNE_Legend <- function(start, end, colors, DNELabels, scaled = F, edgeMask = F, outlierMask = F, logColors = F){
  par(ann=F, mar=c(0,0,0,0))
  layout(matrix(1:2,ncol=2), widths = c(0.75, 0.25))
  plot(1,1, type='n', axes=F)
  if(logColors==TRUE){
    ExpNumbers <- exp(seq(start,end,0.001))
    adjust <- (ExpNumbers-min(ExpNumbers))
    adjust <- adjust/max(adjust)
    adjust <- adjust*end
    colorslist <- hsv(adjust)
  }
  if(logColors==FALSE){
    colorslist <- hsv(seq(start, end, 0.001))
  }
  legend_gradient <- as.raster(matrix(colorslist, ncol=1))
  edgeblack <- as.raster(matrix('#000000'))
  outliergray <- as.raster(matrix('#505050'))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  PlotLabels <- DNELabels
  PlotLabels <- format(PlotLabels, scientific=FALSE)
  text(x=1.25, y = seq(0.25,0.8,l=10), labels = PlotLabels, cex=1.75, adj=c(0, NA))
  rasterImage(legend_gradient, 0.7, 0.25, 1.2,0.8)
  rect(0.7, 0.25, 1.2,0.8, lwd=2)
  segments(x0=rep(0.7, 10), y0=seq(0.25,0.8,l=10), x1=rep(0.8, 10), y1=seq(0.25,0.8,l=10), lwd=2)
  segments(x0=rep(1.1, 10), y0=seq(0.25,0.8,l=10), x1=rep(1.2, 10), y1=seq(0.25,0.8,l=10), lwd=2)
  if(scaled==FALSE && logColors==FALSE){
    text(x=1.25, y=0.875, labels=c("DNE Value\nPer Face"), cex=1.75)
  }
  if(scaled==TRUE && logColors==FALSE){
    text(x=1.25, y=0.875, labels=c("Scaled\nDNE Value\nPer Face"), cex=1.75)
  }
  if(scaled==FALSE && logColors==TRUE){
    text(x=1.25, y=0.875, labels=c("Log DNE Value\nPer Face"), cex=1.75)
  }
  if(scaled==TRUE && logColors==TRUE){
    text(x=1.25, y=0.875, labels=c("Scaled\nLog DNE Value\nPer Face"), cex=1.75)
  }
  if(edgeMask==TRUE && outlierMask==FALSE){
    rasterImage(edgeblack, 0.7, 0.19, 1.2, 0.24)
    rect(0.7, 0.19, 1.2, 0.24, lwd=2)
    text(x=1.25, y=0.215, labels="Edges", cex=1.75, adj=c(0,NA))
  }
  if(edgeMask==FALSE && outlierMask==TRUE){
    rasterImage(outliergray, 0.7, 0.19, 1.2, 0.24)
    rect(0.7, 0.19, 1.2, 0.24, lwd=2)
    text(x=1.25, y=0.215, labels="Outliers", cex=1.75, adj=c(0,NA))
  }
  if(edgeMask==TRUE && outlierMask==TRUE){
    rasterImage(edgeblack, 0.7, 0.19, 1.2, 0.24)
    rect(0.7, 0.19, 1.2, 0.24, lwd=2)
    text(x=1.25, y=0.215, labels="Edges", cex=1.75, adj=c(0,NA))
    rasterImage(outliergray, 0.7, 0.13, 1.2, 0.18)
    rect(0.7, 0.13, 1.2, 0.18, lwd=2)
    text(x=1.25, y=0.155, labels="Outliers", cex=1.75, adj=c(0,NA))  
  }
}
