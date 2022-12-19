#' Make legend for ARC3d plot
#'
#' plotting subfunction
#'
#' @param colors a concatenated string of color names
#' @param size scaling factor for the legend
#'
#' @details This is an internal function which builds a ARC plot legend
#'
#' The legend will reflect the elements used in the plot. This is an internal
#' function. Users will have little need to call or interact with it.
#' ARC_Legend()
#' 
#' @noRd

ARC_Legend <- function(colors=colors, size = 1)
{
  par(ann=F, mar=c(0,0,0,0))
  layout(matrix(1:2,ncol=2), widths = c(0.75, 0.25))
  plot(1,1, type='n', axes=F)
  textSizeFactor <- 1.7 * size
  lineSizeFactor <- 2 * size
  XPos1 <- 1.6                 #X location of center
  XPos2 <- XPos1-(0.45*size)   #X location of left edge legend text
  XPos3 <- XPos1-(0.5*size)    #X location of right edge color box
  XPos4 <- XPos1-(0.8*size)    #X location of left edge color box
  
  YPos0 <- 0.5                 #Y location of center
  
  #rectangles
  YPos1 <- YPos0+(0.3*size)  #Y location of top of top box
  YPos2 <- YPos0+(0.225*size)   
  YPos3 <- YPos0+(0.15*size)  
  YPos4 <- YPos0+(0.075*size)   
  YPos5 <- YPos0-(0.075*size)  
  YPos6 <- YPos0-(0.15*size)
  YPos7 <- YPos0-(0.225*size)
  YPos8 <- YPos0-(0.3*size) # Y location of bottom of bottom box
  
  plot(c(0,2),c(0,1), type = 'n', axes = F, xlab = '', ylab = '')
  
  rect(XPos4, YPos1, XPos3, YPos2, lwd=lineSizeFactor, col=colors[8], border="black")
  rect(XPos4, YPos2, XPos3, YPos3, lwd=lineSizeFactor, col=colors[7], border="black")
  rect(XPos4, YPos3, XPos3, YPos4, lwd=lineSizeFactor, col=colors[6], border='black')
  rect(XPos4, YPos4, XPos3, YPos0, lwd=lineSizeFactor, col=colors[5], border='black')
  rect(XPos4, YPos0, XPos3, YPos5, lwd=lineSizeFactor, col=colors[4], border='black')
  rect(XPos4, YPos5, XPos3, YPos6, lwd=lineSizeFactor, col=colors[3], border='black')
  rect(XPos4, YPos6, XPos3, YPos7, lwd=lineSizeFactor, col=colors[2], border='black')
  rect(XPos4, YPos7, XPos3, YPos8, lwd=lineSizeFactor, col=colors[1], border='black')
  
  YPosT1 <- YPos0+(0.2625*size)
  YPosT2 <- YPos0+(0.1875*size)
  YPosT3 <- YPos0+(0.1125*size)
  YPosT4 <- YPos0-(0.1125*size)
  YPosT5 <- YPos0-(0.1875*size)
  YPosT6 <- YPos0-(0.2625*size)
  text(x=XPos2, y=YPosT1, labels="Extremly \nConvex", cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPosT2, labels="Highly \nConvex", cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPosT3, labels='Convex', cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPos0, labels='Flat', cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPosT4, labels='Concave', cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPosT5, labels='Highly \nConcave', cex=textSizeFactor, adj=c(0,NA))
  text(x=XPos2, y=YPosT6, labels='Extremly \nConcave', cex=textSizeFactor, adj=c(0,NA))
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  