#' Calculate Dirichlet normal energy of a surface
#'
#' A function that calculates Dirichlet normal energy following the method of Bunn et
#' al. (2011) Comparing Dirichlet normal surface energy of tooth crowns, a new
#' technique of molar shape quantification for dietary inference, with previous methods
#' in isolation and in combination. Am J Phys Anthropol 145:247-261 doi: 10.1002
#' ajpa.21489
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d' with calculated normals
#' @param outliers The percentile of Dirichlet energy density values to be excluded 
#' defaults to top 0.1 percent
#' @param kappa An integer value of mean curvature to define concave vs convex faces
#' @param BoundaryDiscard String indicating how to handle the exclusion of 
#' boundary faces. Default of Vertex excludes faces which have at least 1 vertex 
#' on the boundary
#' @param oex String indicating outlier exclusion principle. Defaults to 'c', which 
#' combines all convex and concave faces and removes the percentage of outliers 
#' defined by `outliers`. See details.
#'
#' @details The function requires an object created by reading in a ply file.
#'
#' Dirichlet normal energy is calculated on meshes that represent specimen surfaces and
#' have already been simplified and pre-smoothed in a 3D data editing program. 
#'
#' In the default settings, the function seeks to discard boundary faces. This can be 
#' changed by adjusting the `BoundaryDiscard` argument to 'None' which will not discard 
#' any faces on the boundary. Further, there are two ways of excluding boundary faces.
#' Either if they have a leg on the boundary by setting `BoundaryDiscard='Leg'` or by 
#' excluding any face which has a vertex on the boundary with `BoundaryDiscard='Vertex'`.
#' The function defaults to remove the top 0.1 percent of calculated energy densities as 
#' outliers. Mesh orientation does not affect this calculation.
#' 
#' Faces are labeled as concave or convex on the basis of the `kappa` value, which 
#' defaults to 0. Each face is assigned a `kappa` value, which describes the the localized 
#' degree of convergence or divergence among the three vertex normals on each face. 
#' Faces with positive `kappa` values have vertex normals that are divergent. Faces with 
#'  negative `kappa` values possess vertex normals that are convergent. Users can adjust 
#' the `kappa` value to redefine areas of the tooth assigned to the convex or concave bin.
#' 
#' The mode of Outlier exclusion can be modified with the `oex` argument. The default, 
#'  `oex='c'` considers the Dirichlet energy density values of all faces on the surface and 
#' removes those in the top percentile defined by `outliers`, regardless of the convexity 
#' or concavity bins. The alternative, `oex='s'`, divides the surface into concave and 
#' convex portions, then removes the percentile defined by `outliers` from each of 
#' these subsets before calculating total surface DNE.
#'
#' @importFrom
#' stats quantile aggregate
#'
#' @importFrom
#' Rvcg vcgGetEdge vcgVFadj
#'
#' @import
#' pracma
#'
#' @export
#' DNE
#'
#' @examples
#' DNE_output <- DNE(Tooth)
#' summary(DNE_output)

DNE <- function(plyFile, outliers=0.1, kappa=0, BoundaryDiscard='Vertex', oex='c') {
  if(BoundaryDiscard!='Leg' && BoundaryDiscard!='Vertex' && BoundaryDiscard!='None'){
    stop("BoundaryDiscard must be set to 'Leg' 'Vertex' or 'None'. Select 'None' if you are working with a closed surface.")
  }
  size <- cSize(plyFile$vb[-4,])
  plyFile$vb <- plyFile$vb/size*100
  
  ply <- Equal_Vertex_Normals(plyFile) ## Correct the Vertex Normals Calculation
  Es <- compute_energy_per_face(ply) ## Compute DNE values for each face of the surface
  kapps <- CurveOrientation(plyFile)
  Es <- data.frame(Es, kappas=kapps$kappaface)
    
  if(BoundaryDiscard!='None'){
    ### Extracting and removing Edge Faces
    if(BoundaryDiscard=='Leg') {
      edges <- vcgGetEdge(plyFile)
      temp <- subset(edges, edges$border==1)
      EdgeEs <- sort(as.numeric(temp$facept))
    }
    if(BoundaryDiscard=='Vertex') {
      edges <- vcgGetEdge(plyFile)
      bounds <- subset(edges, edges$border==1)
      edgeverts <- unique(c(bounds$vert1, bounds$vert2))
      list <- vcgVFadj(plyFile)
      EdgeEs <- sort(unique(unlist(list[edgeverts])))
    }
    Boundary_Values <- Es[EdgeEs,] ## This to be Exported, values of edge faces
    Es[EdgeEs,]$Dirichlet_Energy_Densities <- 0
  }
  
  ### Extracting and removing outliers
  outs <- (100-outliers)/100
  if (oex=='c') {
    DNEs <- Es$Dirichlet_Energy_Densities
    FAs <- Es$Face_Areas
    Q <- quantile(DNEs, probs=c(outs))
    Outlier_List <- which(DNEs > Q)
    Outliers <- Es[Outlier_List,] ## This to be Exported, outlier values
    DNEs[Outlier_List] <- 0
  }
  if (oex=='s') {
    splits <- split(Es, Es$kappas>kappa)
    names(splits[1]) <- 'Concaves'
    names(splits[2]) <- 'Convexes'
    
    Qv <- quantile(splits[[2]]$Dirichlet_Energy_Densities, probs=c(outs))
    vOuts <- which(splits[[2]][,1] > Qv)
    
    vOutliers <- splits[[2]][vOuts,]
    
    Qc <- quantile(splits[[1]][,1], probs=c(outs))
    cOuts <- which(splits[[1]][,1] > Qc)
    
    cOutliers <- splits[[1]][cOuts,]
    
    Outliers <- rbind(vOutliers, cOutliers)
    olist <- as.numeric(rownames(Outliers))
    
    Es[olist,1] <- 0
    
    DNEs <- Es[,1]
    FAs <- Es[,2]
    kap <- Es[,3]
  }
  
  ### Concavity-convexity sorting
  kap <- kapps$kappaface
  CleanEs <- data.frame(Dirichlet_Energy_Densities=DNEs, Face_Areas=FAs, Kappa_Values=kap)
  Surface_DNE <- sum(CleanEs$Dirichlet_Energy_Densities*CleanEs$Face_Areas)
  Concaves <- CleanEs[which(kap<=kappa),1]*CleanEs[which(kap<=kappa),2]
  Concave_DNE <- sum(Concaves)
  Convexes <- CleanEs[which(kap>kappa),1]*CleanEs[which(kap>kappa),2]
  Convex_DNE <- sum(Convexes)
  
  CleanEs[,1] <- CleanEs[,1]/size*100
  CleanEs[,2] <- CleanEs[,2]*size/100
  
  Outliers[,1] <- Outliers[,1]/size*100
  Outliers[,2] <- Outliers[,2]*size/100
  
  if(BoundaryDiscard!='None'){
    Boundary_Values[,1] <- Boundary_Values[,1]/size*100
    Boundary_Values[,2] <- Boundary_Values[,2]*size/100
  }
  
  Convex_Area <- sum(CleanEs[which(kap>kappa),2])*size/100
  Concave_Area <- sum(CleanEs[which(kap<=kappa),2])*size/100
  
  nor <- ply$vb[4,]
  vbs <- plyFile$vb*size/100
  vbs <- vbs[-4,]
  rebuild <- rbind(vbs, nor)
  plyFile$vb <- rebuild
  
  if(BoundaryDiscard=='None') {
    Boundary_Values="No Boundary or No Boundary Discard Selected"
  }
  Out <- list(Surface_DNE=Surface_DNE, Convex_DNE=Convex_DNE, Concave_DNE=Concave_DNE, Convex_Area=Convex_Area, Concave_Area=Concave_Area, Kappa=kappa, Face_Values=CleanEs, Boundary_Values=Boundary_Values, Outliers=Outliers, "plyFile"=plyFile)
  cat("Total Surface DNE =", Surface_DNE, "\n")
  cat("Convex DNE=",Convex_DNE,"\n")
  cat("Concave DNE=",Concave_DNE,"\n")
  class(Out) <- 'DNE_Object'
  return(Out)
}