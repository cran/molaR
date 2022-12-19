#' Calculate several measures of Area Relative Curvature
#'
#' A function that calculates the average slope over a tooth
#' or some other 3D surface
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d' with calculated normals
#' @param BoundaryDiscard String indicating how to handle the exclusion of 
#' boundary faces. Default of Vertex excludes faces which have at least 1 vertex 
#' on the boundary 
#' @param Range A pair of values which set lower and upper outlier exclusions.
#'
#'
#' @details The function requires an object created by reading in a ply file.
#'
#' This function calculates Area Relative Curvature, as described by Guy et al. (2013)
#'
#'
#'
#' 
#' @importFrom
#' Rvcg vcgGetEdge vcgVFadj vcgCurve vcgUpdateNormals
#' 
#' 
#'
#' @export
#' ARC
#'
#' @examples
#' arc_output <- ARC(Tooth)
#' summary(arc_output)

ARC <- function (plyFile, BoundaryDiscard='Vertex', Range=c(0.01, 0.99)) 
{
    if(BoundaryDiscard!='Leg' && BoundaryDiscard!='Vertex' && BoundaryDiscard!='None'){
    stop("BoundaryDiscard must be set to 'Leg' 'Vertex' or 'None'. Select 'None' if you are working with a closed surface.")
  }
  if(Range[1]<0 | Range[2]>1) {
  	stop('Range must be between 0 and 1')
  	}
  
  size <- cSize(plyFile$vb[-4,])
  plyFile$vb <- plyFile$vb/size*100
  
  plyFile <- vcgUpdateNormals(plyFile)
  Cs <- vcgCurve(plyFile)$meanitmax/(2.4481/sqrt(vcgArea(plyFile))) 
  
   if(BoundaryDiscard!='None'){
    ### Extracting and removing Edge Faces
    if(BoundaryDiscard=='Leg') {
      edges <- vcgGetEdge(plyFile)
      temp <- subset(edges, edges$border==1)
      EdgeCs <- sort(as.numeric(temp$facept))
    }
    if(BoundaryDiscard=='Vertex') {
      edges <- vcgGetEdge(plyFile)
      bounds <- subset(edges, edges$border==1)
      edgeverts <- unique(c(bounds$vert1, bounds$vert2))
      list <- vcgVFadj(plyFile)
      EdgeCs <- sort(unique(unlist(list[edgeverts])))
    }
    Boundary_Values <- Cs[EdgeCs] ## This to be Exported, values of edge faces
    Cs[EdgeCs] <- 0
  }
  LOW <- quantile(Cs, Range[1])
  HIGH <- quantile(Cs, Range[2])
  Outlier_Values <- list(Low_Outliers = Cs[Cs<LOW], High_Outliers= Cs[Cs>HIGH])
  Cs[Cs<LOW] <- 0
  Cs[Cs>HIGH] <- 0

  out <- list(Mean_Surface_Curvature=mean(Cs), Mean_Positive_Curvature=mean(Cs[Cs >= 0]), Mean_Negative_Curvature=mean(Cs[Cs < 0]), Mean_Face_Curvature=Cs, Boundary_Values=Boundary_Values, Outliers=Outlier_Values, "plyFile"=plyFile)
  cat("Mean ARC =", mean(Cs), "\n")
  cat("Mean Positve ARC=", mean(Cs[Cs>=0]), "\n")
  cat("Mean Negative ARC=", mean(Cs[Cs<0]), "\n")
  return(out)
}