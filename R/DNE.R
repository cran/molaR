#' Calculate Dirichlet normal energy of a surface
#'
#' A function that calculates Dirichlet normal energy following the method of Bunn et
#' al. (2011) Comparing Dirichlet normal surface energy of tooth crowns, a new
#' technique of molar shape quantification for dietary inference, with previous methods
#' in isolation and in combination. Am J Phys Anthropol 145:247-261 doi: 10.1002
#' ajpa.21489
#'
#' @param plyFile An object of class 'mesh3d' and 'shape3d' with calculated normals
#' 
#' @details The function requires an object created by reading in a ply file utilizing
#' either the read.ply or the read.AVIZO.ply function, with calculated normals.
#'
#' Dirichlet normal energy is calculated on meshes that represent specimen surfaces and
#' have already been simplified to 10,000 faces and pre-smoothed in a 3D data
#' editing program. 
#'
#' The function does not include boundary vertices in the calculation, and therefore the
#' analyzed surface cannot be closed (i.e., it must contain a hole). The function removes
#' the top 0.1 percent of calculated energy densities as outliers. Mesh orientation does not
#' affect for this calculation.
#'
#' @importFrom
#' stats quantile aggregate
#'
#' @export
#' DNE





DNE <- function(plyFile) {
	
	ply <- Equal_Vertex_Normals(plyFile) ## Correct the Vertex Normals Calculation
	Es <- compute_energy_per_face(ply) ## Compute DNE values for each face of the surface
	
	
	### Extracting and removing Edge Faces
	EdgeVs <- edge_vertices(ply) ## ID vertices on Edge
	Vert_to_Face <- vertex_to_face_list(ply) ## Match Verts to faces they touch
	
	EdgesTemp <- sort(unlist(Vert_to_Face[EdgeVs]))
	ttemp <- data.frame(table(EdgesTemp))
	Etemp <- ttemp[ttemp$Freq>1,]
	EdgeEEs <- as.vector(Etemp$EdgesTemp)
	EdgeEs <- as.numeric(EdgeEEs)
	
	Edge_Values <- Es[EdgeEs,] ## This to be Exported, values of edge faces
	
	Es[EdgeEs,]$DNE_Values <- 0
	
	### Extracting and removing outliers
	
	DNEs <- Es$DNE_Values
	FAs <- Es$Face_Areas
	Q <- quantile(DNEs, probs=c(0.999))
	
	Outlier_List <- which(DNEs > Q)
	
	Outliers <- Es[Outlier_List,] ## This to be Exported, outlier values
	
	DNEs[Outlier_List] <- 0
	
	CleanEs <- data.frame(DNE_Values=DNEs, Face_Areas=FAs)
	
	Surface_DNE <- sum(CleanEs$DNE_Values*CleanEs$Face_Areas)
	
	Out <- list(Surface_DNE=Surface_DNE, Face_Values=CleanEs, Edge_Values=Edge_Values, Outliers=Outliers, "plyFile"=plyFile)
	print("Total Surface DNE")
	print(Surface_DNE)
	return(Out)
	
}
