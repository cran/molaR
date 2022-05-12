#' Clean up problem ply files
#'
#' Function will remove floating verticies, and faces with zero
#' area. These can cause issues when using molaR's primary
#' functions of DNE, RFI, and OPC
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d'
#' @param cleanType String with three arguments defining what to clean: Vertices,
#' Faces, or Both. Defaults to Both.
#' @param verbose Logical indicating if the function should report changes to ply
#'
#' @details This function cleans up problematic ply files. Some
#' smoothed files will have faces of zero area, or floating
#' vertices. DNE and OPC cannot be calculated on these files.
#' Running the plys through this function will allow those
#' calculations to be made. 
#'
#'
#' @export
#' molaR_Clean
#'
#' @examples
#' Tooth <- molaR_Clean(Tooth)

molaR_Clean <- function(plyFile, cleanType = "Both", verbose=TRUE) 
{
  if (cleanType != "Both" && cleanType != "Faces" && 
      cleanType != "Vertices") {
    stop("cleanType must be set to either 'Faces', 'Vertices', or 'Both'.")
  }
  if (cleanType == "Both") {
    Areas <- vcgArea(plyFile, perface = TRUE)
    Zeroes <- which(Areas$pertriangle == 0)
    if(verbose == TRUE){cat("Removed", length(Zeroes), "faces with area = 0\n")}
    if (length(Zeroes) > 0) {
      plyFile$it <- plyFile$it[, -Zeroes]
      if(verbose == TRUE){cat("Indices of removed faces:", Zeroes, "\n")}
    }
    Verts <- 1:ncol(plyFile$vb)
    VertList <- as.vector(plyFile$it)
    InFaces <- Verts %in% VertList
    NotIn <- sort(which(InFaces == FALSE))
    if(verbose == TRUE){cat("Removed", length(NotIn), "unreferenced vertices from mesh\n")}
    if (length(NotIn) > 0) {
      if(verbose == TRUE){cat("Indices of removed vertices:", NotIn, "\n")}
      for (i in 1:length(NotIn)) {
        plyFile$vb <- plyFile$vb[, -NotIn[i]]
        plyFile$normals <- plyFile$normals[, -NotIn[i]]
        HighFaces <- plyFile$it > NotIn[i]
        plyFile$it[HighFaces] <- plyFile$it[HighFaces] - 1
        NotIn <- NotIn - 1
      }
    }
  }
  if (cleanType == "Vertices") {
    Verts <- 1:ncol(plyFile$vb)
    VertList <- as.vector(plyFile$it)
    InFaces <- Verts %in% VertList
    NotIn <- sort(which(InFaces == FALSE))
    if(verbose == TRUE){cat("Removed", length(NotIn), "unreferenced vertices from mesh\n")}
    if (length(NotIn) > 0) {
      if(verbose == TRUE){cat("Indices of removed vertices:", NotIn, "\n")}
      for (i in 1:length(NotIn)) {
        plyFile$vb <- plyFile$vb[, -NotIn[i]]
        plyFile$normals <- plyFile$normals[, -NotIn[i]]
        HighFaces <- plyFile$it > NotIn[i]
        plyFile$it[HighFaces] <- plyFile$it[HighFaces] - 
          1
        NotIn <- NotIn - 1
      }
    }
  }
  if (cleanType == "Faces") {
    Areas <- vcgArea(plyFile, perface = TRUE)
    Zeroes <- which(Areas$pertriangle == 0)
    if(verbose==TRUE){cat("Removed", length(Zeroes), "faces with area = 0\n")}
    if (length(Zeroes) > 0) {
      plyFile$it <- plyFile$it[, -Zeroes]
      if(verbose==TRUE){cat("Indices of removed faces:", Zeroes, "\n")}
    }
  }
  return(plyFile)
}
