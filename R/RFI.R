#' Calculate Boyer's (2008) relief index for a surface
#'
#' A function that calculates relief index following Boyer (2008) Relief index of
#' second mandibular molars is a correlate of diet among prosimian primates and
#' other mammals. J Hum Evol 55:1118-1137 doi: 10.1016/j.jhevol.2008.08.002
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d'
#' @param alpha Step size for calculating the outline. See details
#'
#' @details The function requires an object created by reading in a ply file utilizing
#' either the read.ply or the read.AVIZO.ply function, with calculated normals.
#' 
#' Relief index is calculated by the ratio of three-dimensional surface area to two
#' dimensional area on meshes that represent specimen surfaces and have already
#' been pre-smoothed in a 3D data editing program. Alignment of the point cloud
#' will have a large effect on patch orientation and must be done in a 3D data editing
#' program or {auto3dgm} prior to creating and reading in the ply file. The mesh
#' must be oriented such that the occlusal plane is parallel to the X- and Y-axes and
#' perpendicular to the Z-axis.
#'
#' Some files may fail with pancake[TempF,] : subscript out
#' of bounds. In these files it may be necessary to increase
#' the alpha value which is default set to 0.06. Increasing the
#' alpha value can cause the RFI function to over-estimate
#' the size of the footprint. Caution should be exercised when
#' troubleshooting by adjusting alpha
#'
#' @importFrom
#' alphahull ahull
#'
#' @importFrom
#' Rvcg vcgArea
#'
#' @export
#' RFI
#'
#' @examples
#' RFI_output <- RFI(Tooth, alpha=0.5)
#' summary(RFI_output)

RFI <- function(plyFile, alpha=0.06) {
  
  # Calculates 3D area of the PLY
  ThreeDArea <- vcgArea(plyFile)
  
  # Create second PLY file and rescale based on centroid size
  pancakePly <- plyFile
  size <- cSize(plyFile$vb[-4, ])
  pancakePly$vb <- pancakePly$vb/size * 100
  
  # Place the barycenter of the PLY file at the origin
  ThreeDVerts <- t(plyFile$vb[-4,])
  x <- ThreeDVerts[, 1] - mean(ThreeDVerts[, 1])
  y <- ThreeDVerts[, 2] - mean(ThreeDVerts[, 2])
  z <- ThreeDVerts[, 3] - mean(ThreeDVerts[, 3])
  Shifted <- as.matrix(cbind(x, y, z))
  ThreeDVerts2 <- t(pancakePly$vb[-4,])
  x2 <- ThreeDVerts2[, 1] - mean(ThreeDVerts2[, 1])
  y2 <- ThreeDVerts2[, 2] - mean(ThreeDVerts2[, 2])
  z2 <- ThreeDVerts2[, 3] - mean(ThreeDVerts2[, 3])
  Shifted2 <- as.matrix(cbind(x2, y2, z2))
  
  # Double-check that there are no vertices in identical location, and drop if any
  Origin <- c(0, 0, 0)
  Shifted <- rbind(Shifted, Origin)
  Xs <- duplicated(Shifted[, 1])
  Ys <- duplicated(Shifted[, 2])
  Pairs <- cbind(Xs, Ys)
  dropps <- which(Pairs[, 1] == T & Pairs[, 2] == T)
  if (length(dropps) > 0) {
    Shifted <- Shifted[-c(dropps), ]
  }
  Shifted2 <- rbind(Shifted2, Origin)
  Xs2 <- duplicated(Shifted2[, 1])
  Ys2 <- duplicated(Shifted2[, 2])
  Pairs2 <- cbind(Xs2, Ys2)
  dropps2 <- which(Pairs2[, 1] == T & Pairs2[, 2] == T)
  if (length(dropps2) > 0) {
    Shifted2 <- Shifted2[-c(dropps2), ]
  }
  
  # Flatten all X, Y points to a single plane, with Z = 0
  pancake <- as.matrix(cbind(Shifted[, 1:2], z = rep(0, length(Shifted[,1]))))
  pancake2 <- as.matrix(cbind(Shifted2[, 1:2], z2 = rep(0, length(Shifted2[,1]))))
  
  # Calcuate alpha hull of the pancake
  hull <- ahull(pancake2[, 1:2], alpha = alpha)
  
  # Isolate end-points of each segment comprising the alpha hull
  arcs <- hull$arcs
  if (length(arcs[, 7]) != length(unique(arcs[, 7]))) {
    stop("alpha adjustment required")
  }
  STedges <- arcs[, "end1"]
  EDedges <- arcs[, "end2"]
  
  # Double-check that there are no 0 length segments
  test <- STedges - EDedges
  if (length(which(test == 0)) > 0) {
    stop("alpha adjustment required")
  }
  
  # Create a series of triangles from origin to segment endpoints
  Or <- length(Shifted[, 1])
  center <- rep(Or, length(STedges))
  slices <- cbind(center, STedges, EDedges)
  
  # Double-check that triangles are non-overlapping
  EdgePts  <- sort(unique(c(STedges, EDedges)))
  AlphaWarning <- FALSE
  for(i in 1:length(EdgePts)){
    PtofInt <- unname(pancake2[EdgePts[i],1:2])
    for(j in 1:nrow(slices)){
      Tri1 <- unname(pancake2[slices[j,1],1:2])
      Tri2 <- unname(pancake2[slices[j,2],1:2])
      Tri3 <- unname(pancake2[slices[j,3],1:2])
      BaryA <- (((Tri2[2]-Tri3[2])*(PtofInt[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(PtofInt[2]-Tri3[2])))/(((Tri2[2]-Tri3[2])*(Tri1[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(Tri1[2]-Tri3[2])))
      BaryB <- (((Tri3[2]-Tri1[2])*(PtofInt[1]-Tri3[1]))+((Tri1[1]-Tri3[1])*(PtofInt[2]-Tri3[2])))/(((Tri2[2]-Tri3[2])*(Tri1[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(Tri1[2]-Tri3[2])))
      BaryG <- 1-BaryA-BaryB
      if(BaryA>0 & BaryB>0 & BaryG>0){
        warning("Possible 2D Area over-estimated due to alpha tracing error. Run Check2D() on output.", immediate. = TRUE)
        AlphaWarning <- TRUE
        break
      }
    }
    if(AlphaWarning==TRUE){break}
  }
  
  # Calculate and sum 2D areas
  TwoDFace_areas <- numeric(length(slices[, 1]))
  for (i in 1:length(TwoDFace_areas)) {
    TempF <- slices[i, ]
    TempV <- pancake[TempF, ]
    b1 <- TempV[2, ]
    b2 <- TempV[3, ]
    g <- matrix(c(sum(b1 * b1), sum(b1 * b2), sum(b2 * b1), 
                  sum(b2 * b2)), nrow = 2)
    TwoDFace_areas[i] <- 0.5 * sqrt(abs(g[1, 1] * g[2, 2] - 
                                          g[1, 2] * g[2, 1]))
  }
  TwoDArea <- sum(TwoDFace_areas)
  
  # Calculate Boyer's RFI and produce output
  RFI <- log(sqrt(ThreeDArea)/sqrt(TwoDArea))
  Out <- list(Surface_RFI = RFI, Three_D_Area = ThreeDArea, 
              Two_D_Area = TwoDArea, Alpha_Warning = AlphaWarning, Translated_Pts = Shifted,
              Flattened_Pts = pancake, Footprint_Triangles = slices, plyFile = plyFile)
  cat("RFI =", RFI, "\n")
  cat("3D Area =", ThreeDArea, "\n")
  cat("2D Area =", TwoDArea, "\n")
  return(Out)
}