#' Plotting subfunction for making slope plot legend
#'
#' @param colors provided from Slope3d
#' @param maskNegatives logical whether to mask faces with negative slopes values
#' @param size Numeric for legend scaling factor
#' @details This is an internal function which builds a Slope plot legend
#'
#' The legend will reflect the elements used in the plot. This is an internal
#' function. Users will have little need to call or interact with it.
#'
#' @noRd


Slope_Legend <- function(colors=colors, maskNegatives=T, size=1) 
{
	colorsfunc <- colorRamp(colors)
	par(ann=F, mar=c(0,0,0,0))
	layout(matrix(1:2,ncol=2), widths=c(0.75, 0.25))
	plot(1,1, type='n', axes=F)
	lineSize <- 2 * size
	textSize <- 1.75 * size
	rectSize <- size
	colorslist <- colorsfunc(rev(seq(0, 1, 0.001)))
	colorslist <- apply(colorslist, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
	legend_gradient <- as.raster(matrix(colorslist, ncol=1))
	XPos1 <- 1.15                     #X location of center (center of title, left edge legend text)
  XPos2 <- XPos1-(0.55*rectSize)    #X location of left edge rectangle
  XPos3 <- XPos1-(0.05*rectSize)    #X location of right edge rectangle
  XPos4 <- XPos1-(0.45*rectSize)    #X location of inside of left tic marks
  XPos5 <- XPos1-(0.15*rectSize)    #X location of inside of right tic marks
  YPos0 <- 0.5                      #Y location of center
  YPos1 <- YPos0+(0.3*rectSize)     #Y location of top of rectangle / legend text
  YPos2 <- YPos0-(0.25*rectSize)    #Y location of bottom of rectangle / legend text
  YPos3 <- YPos0+(0.375*rectSize)   #Y location of title
  YPos4 <- YPos0-(0.27*rectSize)    #Y location of top of box below rectangle
  YPos5 <- YPos0-(0.32*rectSize)    #Y location of bottom of box below rectangle
  YPos6 <- YPos0-(0.295*rectSize)   #Y location of text for box below rectangle
  YPos7 <- YPos0-(0.34*rectSize)    #Y location of top of second box below rectangle
  YPos8 <- YPos0-(0.39*rectSize)    #Y location of bottom of second box below rectangle
  YPos9 <- YPos0-(0.365*rectSize)   #Y location of text for second box below rectangle
  plot(c(0,2), c(0,1), type='n', axes=F, xlab='', ylab='')
  Slope_Labels <- seq(0,90, l=10)
  Slope_Labels <- format(Slope_Labels, scientific=F)
  text(x=XPos1, y=seq(YPos2, YPos1, l=10), labels=Slope_Labels, adj=c(0, NA), cex=textSize)
   rasterImage(legend_gradient, XPos2, YPos2, XPos3,YPos1)
  rect(XPos2, YPos2, XPos3, YPos1, lwd=lineSize)
  segments(x0=rep(XPos2, 10), y0=seq(YPos2,YPos1,l=10), x1=rep(XPos4, 10), y1=seq(YPos2,YPos1,l=10), lwd=lineSize)
  segments(x0=rep(XPos5, 10), y0=seq(YPos2,YPos1,l=10), x1=rep(XPos3, 10), y1=seq(YPos2,YPos1,l=10), lwd=lineSize)
  
  if(maskNegatives==T){
  	edgeblack <- '#000000'
  	rect(XPos2, YPos5, XPos3, YPos4, lwd=lineSize, col=edgeblack, border="black")
    text(x=XPos1, y=YPos6, labels="Negative\nSlope", cex=textSize, adj=c(0,NA))
  }
}
