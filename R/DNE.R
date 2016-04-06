#' Calculate Dirichlet normal energy of a surface
#'
#' A function that calculates Dirichlet normal energy following the method of Bunn et
#' al. (2011) Comparing Dirichlet normal surface energy of tooth crowns, a new
#' technique of molar shape quantification for dietary inference, with previous methods
#' in isolation and in combination. Am J Phys Anthropol 145:247-261 doi: 10.1002
#' ajpa.21489
#'
#' @param plyFile An object of class 'mesh3d' and 'shape3d' with calculated normals
#' @param outliers The percentile of Dirichlet energy density values to be excluded 
#' defaults to top 0.1 percent
#'
#' @details The function requires an object created by reading in a ply file utilizing
#' either the read.ply or the read.AVIZO.ply function, with calculated normals.
#'
#' Dirichlet normal energy is calculated on meshes that represent specimen surfaces and
#' have already been simplified to 10,000 faces and pre-smoothed in a 3D data
#' editing program. 
#'
#' The function does not include boundary vertices in the calculation, and therefore the
#' analyzed surface cannot be closed (i.e., it must contain a hole). The function defaults to
#' remove the top 0.1 percent of calculated energy densities as outliers. Mesh orientation
#' does not affect for this calculation.
#'
#' @importFrom
#' stats quantile aggregate
#'
#' @importFrom
#' Rvcg vcgGetEdge
#'
#' @export
#' DNE





DNE <- function(plyFile, outliers=0.1) {
	
	size <- cSize(plyFile$vb)
	plyFile$vb <- plyFile$vb/size*100
	
	ply <- Equal_Vertex_Normals(plyFile) ## Correct the Vertex Normals Calculation
	Es <- compute_energy_per_face(ply) ## Compute DNE values for each face of the surface
	
	
	### Extracting and removing Edge Facess
	edges <- vcgGetEdge(plyFile)
	temp <- subset(edges, edges$border==1)
	EdgeEs <- sort(as.numeric(temp$facept))
	
	Boundary_Values <- Es[EdgeEs,] ## This to be Exported, values of edge faces
	
	Es[EdgeEs,]$Dirichlet_Energy_Densities <- 0
	
	### Extracting and removing outliers
	outs <- (100-outliers)/100
	
	DNEs <- Es$Dirichlet_Energy_Densities
	FAs <- Es$Face_Areas
	Q <- quantile(DNEs, probs=c(outs))
	
	Outlier_List <- which(DNEs > Q)
	
	Outliers <- Es[Outlier_List,] ## This to be Exported, outlier values
	
	DNEs[Outlier_List] <- 0
	
	CleanEs <- data.frame(Dirichlet_Energy_Densities=DNEs, Face_Areas=FAs)
	
	Surface_DNE <- sum(CleanEs$Dirichlet_Energy_Densities*CleanEs$Face_Areas)
	
	CleanEs[,1] <- CleanEs[,1]/size*100
	CleanEs[,2] <- CleanEs[,2]*size/100
	
	Outliers[,1] <- Outliers[,1]/size*100
	Outliers[,2] <- Outliers[,2]*size/100
	
	Boundary_Values[,1] <- Boundary_Values[,1]/size*100
	Boundary_Values[,2] <- Boundary_Values[,2]*size/100
	
	plyFile$vb <- plyFile$vb*size/100
	
	Out <- list(Surface_DNE=Surface_DNE, Face_Values=CleanEs, Boundary_Values=Boundary_Values, Outliers=Outliers, "plyFile"=plyFile)
	print("Total Surface DNE")
	print(Surface_DNE)
	return(Out)
	
}