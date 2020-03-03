#' Plot 2D footprint and footprint triangle points to check for erros in 2D calculation
#'
#' This function will plot the points used for the 2D footprint area
#' calculation. This is meant to be a visual checking mechanism to
#' ensure that there are no 'extra' triangles within the footprint erroneously
#' adding to the total 2D area of the footprint. If a user finds extra points
#' within the boundaries of the footprint, they should assume that the alpha
#' value used for the RFI calculation was too small, and they are getting
#' a 2D footprint calculation which was too large. 
#' 
#' @param RFI_Output An object that stores the output of the RFI
#' function
#' @param FootColor changes color of the 2D surface footprint
#' @param TriPointsColor color for the points of the footprint triangles
#' 
#' 
#' @details This function will plot the points used for the 2D footprint area
#' calculation. This is meant to be a visual checking mechanism to
#' ensure that there are no 'extra' triangles within the footprint erroneously
#' adding to the total 2D area of the footprint. If a user finds extra points
#' within the boundaries of the footprint, they should assume that the alpha
#' value used for the RFI calculation was too small, and they are getting
#' a 2D footprint calculation which was too large. 
#'
#' @import
#' rgl
#' 
#' @export
#' Check2D
#'
#' @examples
#' RFI_output <- RFI(Tooth, alpha=0.5)
#' Check2D(RFI_output)


Check2D <- function(RFI_Output, FootColor='red', TriPointsColor='black'){
	
	tris <- RFI_Output$Footprint_Triangles
	pt1 <- RFI_Output$Flattened_Pts[tris[,1],]
	pt2 <- RFI_Output$Flattened_Pts[tris[,2],]
	pt3 <- RFI_Output$Flattened_Pts[tris[,3],]
	
	RFI3d(RFI_Output, displacement=0, Opacity=0, FootColor=FootColor)
	points3d(pt1, col=TriPointsColor)
	points3d(pt2, col=TriPointsColor)
	points3d(pt3, col=TriPointsColor)
	
}



