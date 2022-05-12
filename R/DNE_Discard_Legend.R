#' Make legend for DNE3d plot
#'
#' plotting subfunction
#'
#' @param Title Title for the plot
#' @param Base color for the base of the plot
#' @param Bound color for the boundary faces excluded from DNE calculation
#' @param Outlier color for the outlier faces excluded from DNE calculation
#' @param Concave color for the concave portions of the surface, if equal to Base
#' function is disabled
#' @param size legend scaling factor
#'
#' @details This is an internal function which builds a better DNE plot legend
#'
#' The legend will reflect the elements used in the plot. This is an internal
#' function. Users will have little need or call to interact with it.
#' 
#' @noRd

DNE_Discard_Legend <- function(Base, Bound, Outlier, Concave, size=1){
  par(ann=F, mar=c(0,0,0,0))
  layout(matrix(1:2,ncol=2), widths = c(0.75, 0.25))
  plot(1,1, type='n', axes=F)
  start <- 0
  end <- 0.575
  lineSize <- 2 * size
  textSize <- 1.75 * size
  rectSize <- size
  XPos1 <- 1.15                     #X location of center (center of title, left edge legend text)
  YPos0 <- 0.5                      #Y location of center
  XPos2 <- XPos1-(0.55*rectSize)    #X location of left edge rectangle
  XPos3 <- XPos1-(0.05*rectSize)    #X location of right edge rectangle
  YPos4 <- YPos0-(0.27*rectSize)    #Y location of top of box below rectangle
  YPos5 <- YPos0-(0.32*rectSize)    #Y location of bottom of box below rectangle
  YPos6 <- YPos0-(0.295*rectSize)   #Y location of text for box below rectangle
  YPos7 <- YPos0-(0.34*rectSize)    #Y location of top of second box below rectangle
  YPos8 <- YPos0-(0.39*rectSize)    #Y location of bottom of second box below rectangle
  YPos9 <- YPos0-(0.365*rectSize)   #Y location of text for second box below rectangle
  YPos10 <- YPos0 -(0.25*rectSize)  #Y location of bottom box of concave rectangle
  YPos11 <- YPos0 -(0.20*rectSize)  #Y location of top of box of concave rectangle
  YPos12 <- YPos0 -(0.225*rectSize) #Y location of text for concave
  
  
  plot(c(0,2),c(0,1), type = 'n', axes = F,xlab = '', ylab = '')

    rect(XPos2, YPos5, XPos3, YPos4, lwd=lineSize, col=Bound, border="black")
    text(x=XPos1, y=YPos6, labels="Edges", cex=textSize, adj=c(0,NA))
    rect(XPos2, YPos8, XPos3, YPos7, lwd=lineSize, col=Outlier, border="black")
    text(x=XPos1, y=YPos9, labels="Outliers", cex=textSize, adj=c(0,NA))
    
    if (Concave!=Base) {
    rect(XPos2, YPos10, XPos3, YPos11, lwd=lineSize, col=Concave, border='black')  
    text(x=XPos1, y=YPos12, labels='Concave', cex=textSize, adj=c(0,NA))
    }
}