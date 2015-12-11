#' Plot results of OPC analysis of a surface
#'
#' A function that produces a three-dimensional rendering of face
#' orientation on a surface. The OPC function will identify the
#' orientations of mesh faces and assign them to patches. It must be
#' performed prior to using the OPC3d function. 
#'
#' @param OPC_Output_Object An object that stores the output of
#' the OPC function 
#' @param fieldofview Passes an argument to par3d changing the
#' field of view in dregrees of the resulting rgl window
#' @param legend Logical indicating whether or not a legend should
#' be displayed
#' @param binColors Allows the user to change the colors filled in for
#' each directional bin
#' @param patchOutline logical whether or not to outline the patches
#' @param outlineColor parameter designating which color to outline the patches in
#' @param maskDiscard logical indicating whether to discard the unused patches
#' @param minimum_faces value for the minimum number of faces a patch must contain
#' to avoid being discarded
#' 
#' @details This function will assign a uniform color to all faces on the mesh
#' surface that share one of the 8 orientations identified by the OPC function. The
#' function returns a colored shade3d of the mesh so that patches can be visually
#' inspected. Future versions will include the option to black out patches not
#' included in the orientation patch count.
#' 
#' fieldofview is set to a default of 0, which is an isometric projection. Increasing it
#' alters the degree of parallax in the perspective view, up to a maximum of 179
#' degrees.

#' colors will support any vector of 8 colors, in any coloration scheme. Default
#' draws from the hsv color space to evenly space color information, however user
#' can supply a list of RGB values, character strings, or integers in place.
#'
#' @import
#' rgl
#'
#' @export
#' OPC3d


OPC3d <- function (OPC_Output_Object, fieldofview = 0, legend = TRUE, 
          binColors = hsv(h = (seq(10, 290, 40)/360), s = .9, v = .85),
          patchOutline = FALSE, outlineColor="black", maskDiscard = FALSE, minimum_faces=3) 
{
  plyFile <- OPC_Output_Object$plyFile
  fieldofview = fieldofview
  bins <- plyFile$Directional_Bins
  BinCount <- as.numeric(length(unique(plyFile$Directional_Bins)))
  BlackPatch <- NULL
  for(i in 1:BinCount){
    Bin <- which(bins == i)
    bins[Bin] <- binColors[i]
    if(maskDiscard==TRUE){
      PatchList <- unlist(OPC_Output_Object$Patches[i], recursive=F)
      SmallPatch <- names(which(lapply(PatchList, length) < minimum_faces))
      Discarded <- as.numeric(unlist(PatchList[SmallPatch]))
      BlackPatch <- c(BlackPatch, Discarded)
    }
  }
  colormatrix <- bins
  if(maskDiscard==TRUE){
    colormatrix[BlackPatch] <- '#000000'
  }
  colormatrix <- rep(colormatrix, 3)
  colormatrix <- matrix(colormatrix, nrow = 3, byrow = T)
  open3d()
  par3d(windowRect = c(100, 100, 900, 900))
  rgl.viewpoint(fov = fieldofview)
  if(legend == TRUE) {
    Fills <- rep('#FFFFFF', BinCount)
    for(i in 1:BinCount){
      Fills[i] <- binColors[i]
    }
    legend3d(x = "right", legend = c(1:8), fill = Fills, 
             title = "Orientations\nby Bin", bty = "n", cex = 1.75)
    if(maskDiscard == TRUE){
      legend3d(x = "right", legend = c(1:8, "Discarded"), fill = c(Fills, '#000000'), 
               title = "Orientations\nby Bin", bty = "n", cex = 1.75)
    }
  }
  if(patchOutline==TRUE){
    for(i in 1:BinCount){
      Orientation <- OPC_Output_Object$Patches[i]
      PatchCount <- as.numeric(length(Orientation[[1]]))
      for(j in 1:PatchCount){
        Patch <- Orientation[[1]][j]
        Patch <- as.numeric(Patch[[1]])
        Faces <- t(plyFile$it[,Patch])
        fnum <- length(Faces[,1])
        vorder <- vector('list', fnum)
        for (i in 1:fnum) {
          vorder[[i]] <- unlist(sort(Faces[i,]))
        }
        edges <- vector('list', fnum)
        for (i in 1:fnum) {
          Ordered <- vorder[[i]]
          G1 <- Ordered[1]
          G2 <- Ordered[2]
          G3 <- Ordered[3]
          ED1 <- paste(G1, G2, sep='_')
          ED2 <- paste(G1, G3, sep='_')
          ED3 <- paste(G2, G3, sep='_')
          edges[[i]] <- paste(ED1, ED2, ED3, sep=',')
        }
        for (i in 1:fnum) {
          edges[[i]] <- unlist(strsplit(edges[[i]], ','))
        }
        string <- unlist(edges)
        edgeframe <- data.frame(names=string)
        UniqueEdge <- aggregate(edgeframe, list(edgeframe$names), FUN=length)
        PatchEdge <- subset(UniqueEdge, UniqueEdge$names==1)
        EdgeVerts <- as.numeric(unlist(strsplit(as.character(unlist(PatchEdge$Group.1)), '_')))
        EdgeCoords <- plyFile$vb[1:3,EdgeVerts]
        segments3d(t(EdgeCoords), color=outlineColor, lwd=1.25, shininess=120)
      }
    }
  }
  shade3d(plyFile, color = colormatrix, shininess=120)
}

