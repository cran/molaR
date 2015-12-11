#' Plot 3D and 2D areas of a mesh used to calculate relief index
#'
#' A function that plots a three-dimensional model of the mesh
#' surface and includes a footprint of the two-dimensional area for
#' visual comparison. 
#' 
#' @param RFI_Output An object that stores the output of the RFI
#' function
#' @param displace Moves the surface footprint up, down, or not at
#' all
#' @param SurfaceColor changes the color of the 3D surface mesh
#' @param FootColor changes color of the 2D surface footprint
#' @param fieldofview Passes an argument to par3d changing the
#' field of view in degrees of the resulting rgl window
#' @param Transparency adjusts the transparency of the 3D mesh
#' surface
#' @param legend Logical indicating whether or not to include a
#' legend of the colors chosen to represent the 3D surface and
#' footprint
#' 
#' @details This function can help to visualize the three-dimensional and two
#' dimensional areas that are used in calculating the relief index of a surface by
#' displaying both at the same time. The RFI function must be performed first.
#' 
#' Transparency can be adjusted in a range from fully opaque (1) to fully
#' transparent (0) in order to help visualize the footprint. The vertical placement of
#' the footprint along the Z axis can be altered with displace depending on how the
#' user wishes to view the surface, or on the original mesh orientation.
#' 
#' fieldofview is set to a default of 0, which is an isometric projection. Increasing it
#' alters the degree of parallax in the perspective view, up to a maximum of 179
#' degrees.
#'
#' @import
#' rgl
#' 
#' @export
#' RFI3d



RFI3d <- function(RFI_Output, displace='Up', SurfaceColor='gray', FootColor='red', fieldofview=0, Transparency=1, legend=F) {
	plyFile <- RFI_Output$plyFile
	## pull out visual parameters
	alpha = Transparency
	ToothColor= SurfaceColor
	FootColor = FootColor
	
	## Basic Plotting functions
	open3d()
	par3d(windowRect=c(100, 100, 900, 900)) ## Window size consistent with DNE3d
	rgl.viewpoint(fov=fieldofview)
	
	
	### replacing translated points into the plyFile
	plyFile <- plyFile
	newPoints1 <- RFI_Output$Translated_Pts
	newPoints2 <- cbind(newPoints1, rep(1, length(newPoints1[,1])))
	newPoints3 <- t(newPoints2)
	
	plyFile$vb <- newPoints3
	
	
	### Plotting the ply file
	shade3d(plyFile, color=ToothColor, alpha=alpha)
	
	### Preparing the footprint and plotting the displaced footprint
	if(displace=="Up"){
		FootprintPts1 <- RFI_Output$Flattened_Pts
		xy <- FootprintPts1[,1:2]
		displaceA <- max(plyFile$vb[3,])
		displaceB <- min(plyFile$vb[3,])
		displace <- 0.90*(displaceA - displaceB)
		z <- FootprintPts1[,3]-displace
		xyz <- cbind(xy, z)
		FootprintPts2 <- cbind(xyz, rep(1, length(z)))
		FootprintPts3 <- t(FootprintPts2)
		
		triangles <- t(RFI_Output$Footprint_Triangles)
		
		Footprint <- list(vb=FootprintPts3, it = triangles, primitivetype='triangle', material=NULL)
		class(Footprint) <- c('mesh3d', 'shape3d')
		shade3d(Footprint, color=FootColor)
	}
	
	if(displace=="Down"){
		FootprintPts1 <- RFI_Output$Flattened_Pts
		xy <- FootprintPts1[,1:2]
		displaceA <- max(plyFile$vb[3,])
		displaceB <- min(plyFile$vb[3,])
		displace <- 0.90*(displaceB - displaceA)
		z <- FootprintPts1[,3]-displace
		xyz <- cbind(xy, z)
		FootprintPts2 <- cbind(xyz, rep(1, length(z)))
		FootprintPts3 <- t(FootprintPts2)
		
		triangles <- t(RFI_Output$Footprint_Triangles)
		
		Footprint <- list(vb=FootprintPts3, it = triangles, primitivetype='triangle', material=NULL)
		class(Footprint) <- c('mesh3d', 'shape3d')
		shade3d(Footprint, color=FootColor)
	}
	
	if(displace=="None"){
		FootprintPts1 <- RFI_Output$Flattened_Pts
		xy <- FootprintPts1[,1:2]
		displaceA <- max(plyFile$vb[3,])
		displaceB <- min(plyFile$vb[3,])
		displace <- 0*(displaceA - displaceB)
		z <- FootprintPts1[,3]-displace
		xyz <- cbind(xy, z)
		FootprintPts2 <- cbind(xyz, rep(1, length(z)))
		FootprintPts3 <- t(FootprintPts2)
		
		triangles <- t(RFI_Output$Footprint_Triangles)
		
		Footprint <- list(vb=FootprintPts3, it = triangles, primitivetype='triangle', material=NULL)
		class(Footprint) <- c('mesh3d', 'shape3d')
		shade3d(Footprint, color=FootColor)
	}	
	
	## in case you want a legend
	if(legend==T){
	legend3d(x='right', legend=c('3D Surface', '2D Foot Print'), fill=c(ToothColor, FootColor))
	}
	
	
}
