#' Function to calculate the average slope of a surface
#'
#' A function that calcualate the average slope over tooth
#' or some other surface
#'
#' @param plyFile An oject of classes 'mesh3d' and 'shape3d'
#' @param Guess Logical indicating whether the function should 
#' 'guess' as to the 'up' direction for the surface and to remove negative
#' slopes from the calculation
#' 
#' @details This function requires a PLY file. It will calculate the slope on each face
#' of the surface and will average the slope across the surface. This is functionally
#' equivalent to the slope calculation used by Ungar and M'Kirera (2003).
#'
#' In the case of applying this function to teeth (its intended purpose) the function expects a
#' surface with the occlusal plane normal to the Z-axis. 
#' 
#' The 'Guess' parameter is a logical asking whether or not you want the function to both
#' guess as to the right side up of the surface, and to then discard all of the 'negative' slopes
#' i.e. surfaces which are over-hangs, as is frequently found on the sidewalls of teeth. If 
#' 'Guess' is not engaged the mean slope will include the negative values of the overhang
#' and will likely underestimate the average slope of the surface. 
#' 
#' Regardless of if the 'Guess' parameter is engaged, the function will also return a vector
#' containing all of the face slope values ("Face_Slopes")
#'
#' @export
#' Slope


Slope <- function(plyFile, Guess=F) {
	plyFile <- Face_Normals(plyFile)
	plyFile <- face_areas(plyFile)
	
	
	Norms <- plyFile$Face_Normals
	Face_Slope <- round(asin(Norms[3,])*(180/pi))
	plyFile$Face_Slopes <- Face_Slope
	
	if(Guess==F){
	Slopes <- Face_Slope*plyFile$Face_Areas
	Mean_Slope <- sum(Slopes)/sum(plyFile$Face_Areas)
	
	if(Mean_Slope>0) {
	out <- list("Mean_Surface_Slope"=abs(Mean_Slope), "plyFile"=plyFile)
	cat('Average Surface Slope=', abs(Mean_Slope), '\n')
	}
	if(Mean_Slope<0) {
		out <- list("Mean_Surface_Slope"=abs(Mean_Slope), "Surface Inversion"="Surface is likely Inverted", "plyFile"=plyFile)
	cat('Average Surface Slope=', abs(Mean_Slope), '\n')
		cat('Your surface is either upside down or inside out', '\n')
		cat('Therefore Mean Slope is returned as absolute value', '\n')
		cat('The signs on the individual face slopes are unchanged')
	}
	}
	if(Guess==T){
		if(sum(Face_Slope)>0) {
			Face_Slope[which(Face_Slope<0)] <- 0
			Areas <- plyFile$Face_Areas
			Areas[which(Face_Slope<0)] <- 0
			Mean_Slope <- sum(Face_Slope*Areas)/sum(Areas)
			out <- list('Mean_Surface_Slope'=Mean_Slope, 'plyFile'=plyFile)
			cat('Average Surface Slope=', Mean_Slope, '\n')
			cat('Function has Guessed that your surface is upright and', '\n')
			cat('that you wanted all negative slopes (overhangs) removed')
		}
		if(sum(Face_Slope)<0) {
			Face_Slope <- Face_Slope*-1
			Face_Slope[which(Face_Slope<0)] <- 0
			Areas <- plyFile$Face_Areas
			Areas[which(Face_Slope<0)] <- 0
			Mean_Slope <- sum(Face_Slope*Areas)/sum(Areas)
			out <- list('Mean_Surface_Slope'=Mean_Slope, "Surface Inversion"="Surface is likely Inverted", 'plyFile'=plyFile)
			cat('Average Surface Slope=', Mean_Slope, '\n')
			cat('Function has Guessed that your surface is upside down and', '\n')
			cat('it has flipped it and removed the negative slopes (overhangs)')
		}
		
	}
	
	return(out)
}