#' Calculate orientation patch count of a surface
#'
#' A function that bins patches of a mesh surface that share general orientation and
#' sums the number of unique patches given certain parameters Modified into
#' 3D from the original 2.5D method described by Evans et al. (2007) High-level
#' similarity of dentitions in carnivorans and rodents. Nature 445:78-81 doi:
#' \href{https://www.nature.com/articles/nature05433}{10.1038/nature05433}
#'
#' @param plyFile An object of classes "mesh3d" and "shape3d" with
#' calculated vertex normals 
#' @param rotation Rotates the file in degrees about the center vertical
#' axis
#' @param minimum_faces Minimum number of ply faces required
#' for a patch to be counted towards the total patch count
#' @param minimum_area Minimum proportion (100%=1.0) of total surface area a
#' patch must occupy to be counted towards the total patch count
#' 
#' @details The function requires a mesh object created by reading in a ply file utilizing
#' either the, \code{\link[Rvcg]{vcgPlyRead}} function.
#' 
#' Orientation patch count is calculated on meshes that represent specimen surfaces
#' and have already been downsampled to 10,000 faces and pre-smoothed in a 3D
#' data editing program. Alignment of the surface will have a large effect on patch
#' orientation and must be performed in a 3D data editing program such as Avizo. The
#' occlusal surface of the specimen must be made parallel to the X- and Y-axes and
#' perpendicular to the Z-axis.
#' 
#' The default for minimum_faces is to ignore patches consisting of two or fewer faces
#' on the mesh. Changing the minimum_area value will disable minimum_faces.
#'
#' @export
#' OPC
#'
#' @examples
#' OPC_output <- OPC(Tooth)
#' summary(OPC_output)


OPC <- function(plyFile, rotation=0, minimum_faces=3, minimum_area=0) {
  
  ### First, Add Face Normals ###
  plyFile <- Face_Normals(plyFile)
  
  ### Add Directional Bins ###
  rotation <- rotation
  plyFile <- Directional_Bins(plyFile, rotation)
  
  ### Add Face Areas ###
  Areas <- vcgArea(plyFile, perface = TRUE)
  plyFile$Face_Areas <- Areas$pertriangle
  
  ### Create Indexed Pairs of Faces, Sorted by Directional Bins ###
  indexed_pairs <- index_paired_directed_faces(plyFile)
  
  ### Cluster Patches for Each Orientation ###
  binned_patches <- patches_for_each_direction(indexed_pairs)
  
  ### Pull Details of Each Patch ###
  patch_details. <- patch_details(binned_patches, plyFile)
  
  ### Patch Counting ###
  minimum_faces <- minimum_faces
  minimum_area <- minimum_area
  patch_count <- patches_per(patch_details., plyFile, minimum_faces, minimum_area)
  
  ### Recording User Set Parameters ###
  Rot <- rotation
  MinFace <- minimum_faces
  MinArea <- minimum_area
  params <- list(Degrees_Rotated= Rot, Minimum_Faces=MinFace, Minimum_Area=MinArea)
  
  
  out <- list("Patch_Count"=patch_count, "Patch_Details"=patch_details., "plyFile"=plyFile, "Patches"=binned_patches, "Parameters"=params)
  BinSort <- out$Patch_Count$directions[order(row.names(out$Patch_Count$directions)),]
  cat("Total Number of Patches =", out$Patch_Count$'total patches')
  cat("\nNumber of Patches per Directional Bin =")
  for(i in 1:length(BinSort)){
    cat("\nBin ", i, ": ", BinSort[i], sep="")
  }
  cat("\n")
  return(out)
  
}