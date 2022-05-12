#' Function for building a legend in OPC plots
#' 
#' Crucial graphics sub-function
#' 
#' @param binColors number sequence for bins and their colors
#' @param binSize numeric of patches in each directional bin
#' @param scaleLegend logical determining legend shape
#' @param maskDiscard logical determining whether faces will be blacked out because
#' they are discarded
#' @param textCol color for the text in the circle legend
#' @param size scaling factor for the legend size
#' @param lineCol color for the lines in the legend
#'
#' @noRd

OPC_Legend <- function(binColors=c(1:8), binSize=c(1:8), scaleLegend = FALSE, 
                       maskDiscard = FALSE, textCol="black", size = 1, lineCol="black"){
  binNumber <- length(binSize)
  textSizeFactor <- 1.75*size
  lineSizeFactor <- 2*size
  par(ann=F, mar=c(0,0,0,0))
  plot(1,1, type='n', axes=F)
  thetas <- seq(pi, 3*pi, length = binNumber+1)
  labelthetas <- thetas+((2*pi)/(2*binNumber))
  XPos1 <- 1.3                  #X location of center of circle: Min = 0.725, Max = 1.3
  XPos2 <- XPos1+(0.115*size)   #X location of +X label
  XPos3 <- XPos1-(0.115*size)   #X location of -X label
  XPos4 <- XPos1-(0.025*size)   #X location of left of Discarded box
  XPos5 <- XPos1-(0.005*size)   #X location of right of Discarded box
  YPos1 <- 1.0125               #Y location of center of circle: Min = 0.725, Max = 1.3
  YPos2 <- YPos1+(0.15*size)    #Y location of title
  YPos3 <- YPos1+(0.115*size)   #Y location of +Y label
  YPos4 <- YPos1-(0.115*size)   #Y location of -Y label
  YPos5 <- YPos1-(0.132*size)   #Y location of top of Discarded box
  YPos6 <- YPos1-(0.152*size)   #Y location of bottom of Discarded box
  YPos7 <- YPos1-(0.142*size)   #Y location of Discarded text
  radius <- 0.1*size            #Radius size of circle
  if(scaleLegend==FALSE){radii <- rep(radius, binNumber)}
  if(scaleLegend==TRUE){radii <- radius*binSize/max(binSize)}
  start <- pi
  end <- (2*pi/binNumber)+pi
  for(i in 1:binNumber){
    Edge <- seq(from=start, to=end, length=200)
    EdgeX <- cos(Edge)*radii[i]+XPos1
    EdgeY <- sin(Edge)*radii[i]+YPos1
    polygon(x=c(XPos1, EdgeX, XPos1), y=c(YPos1, EdgeY, YPos1), col=binColors[i], border=NA)
    polygon(x=c(XPos1, EdgeX, XPos1), y=c(YPos1, EdgeY, YPos1), lwd=lineSizeFactor, border=lineCol)
    start <- start+(2*pi/binNumber)
    end <- end+(2*pi/binNumber)
  }
  text(x=XPos1, y=YPos2, labels=c("Orientation Bins"), cex=textSizeFactor)
  text(x=XPos1, y=YPos3, labels=c("+Y"), cex=0.75*textSizeFactor)
  text(x=XPos1, y=YPos4, labels=c("-Y"), cex=0.75*textSizeFactor)
  text(x=XPos2, y=YPos1, labels=c("+X"), cex=0.75*textSizeFactor)
  text(x=XPos3, y=YPos1, labels=c("-X"), cex=0.75*textSizeFactor)
  minRad <- 0.75*min(radii)
  text(x=(cos(labelthetas)*minRad+XPos1), y=(sin(labelthetas)*minRad+YPos1),
       labels=c(1:binNumber), cex=0.75*textSizeFactor, col=textCol)
  if(maskDiscard==TRUE){
    rect(XPos4, YPos5, XPos5, YPos6, lwd=lineSizeFactor, col="black")
    text(x=XPos1, y=YPos7, labels="Discarded", cex=textSizeFactor, adj=c(0,NA))
  }
}