#' Plot results of OPC analysis of a surface
#'
#' A function that produces a three-dimensional rendering of face
#' orientation on a surface. The `OPC` function will identify the
#' orientations of mesh faces and assign them to patches. It must be
#' performed prior to using the OPC3d function. 
#'
#' @param OPC_File An object that stores the output of the `OPC()` function
#' @param binColors Allows the user to define the fill colors for
#' each directional bin 
#' @param patchOutline Logical whether or not to outline the patches
#' @param outlineColor Parameter defining the patch outline color
#' @param maskDiscard Logical indicating whether or not to mask (in black) the
#' patches excluded from the OPC value
#' @param legend Logical indicating whether or not a legend should
#' be displayed
#' @param main String indicating plot title
#' @param cex Numeric setting the relative size of the legend and title
#' @param scaleLegend Logical indicating if legend bins should scale to patch counts
#' @param legendTextCol Parameter defining color for the legend text
#' @param legendLineCol Parameter defining the color for the legend lines
#' @param leftOffset Numeric between -1 and 1 setting the amount of offset for
#' the plotted surface to the left. Larger values push surface farther to right.
#' @param fieldofview Passes an argument to `par3d()` changing the field of
#' view in degrees of the resulting surface plot
#' @param fileName String indicating a name to save the plotted surface to as a
#' *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should
#' be binary, passed to `vcgPlyWrite()`
#' 
#' @details This function will assign a uniform color to all faces on the mesh
#' surface that share one of the orientation bins identified by the OPC function. The
#' function returns a colored mesh so that patches can be visually inspected.
#' 
#' `binColors` will support any vector of colors, in any coloration scheme. Default
#' draws from the HSV color space to evenly space color information, however the user
#' can supply a list of RGB values or character strings in place. If there are fewer
#' colors than directional bins, remaining bins will default to white.
#' 
#' A title can be added to the plot by supplying a character string to the `main`
#' argument. Title and legend size are controlled with the `cex` argument,
#' analogous to that in the default R graphics device.
#' 
#' Several legend plotting options are available. The default legend shape is a
#' circular pie with sectors indicating the orientation of directional bins, shaded
#' according to the color scheme in `binColors`. By setting `scaleLegend = TRUE`,
#' the legend sectors will scale proportionally to the number of patches in each
#' directional bin. The legend text and line colors can be customized with
#' `legendTextCol` and `legendLineCol`, which both default to black.
#' 
#' The `leftOffset` value sets how far to the left the surface will plot, intended
#' to help avoid overlap with the legend. Value of 0 will center the surface and
#' should be invoked if the `legend` argument is disabled. Higher values will push
#' the surface farther left and negative values will push it to the right. It is
#' recommended that these values be restricted between -1 and 1 to avoid plotting
#' the surface outside of the rgl window.
#' 
#' `fieldofview` is set to a default of 0, which is an isometric projection.
#' Increasing it alters the degree of parallax in the perspective view, up to
#' a maximum of 179 degrees (see \code{\link[rgl:par3d]{rgl::par3d()}}).
#' 
#' The plotted, colorized surface can be saved as a *.ply to the working directory
#' by changing the `fileName` argument from `NA` to a string (e.g., "OPCPlot"). The
#' resultant ply file can be opened and manipulated in other 3D visualizing programs,
#' such as \href{https://www.meshlab.net/}{MeshLab}, but will **NOT** retain its legend
#' (a background of the plotting window). To retain the legend, the user is 
#' encouraged to utilize the \code{\link[rgl:snapshot3d]{rgl::snapshot3d()}} function. 
#' The `binary` argument saves a file in ascii format by default, which is supported by 
#' more 3D visualization software than is binary. However, binary files will be
#' considerably smaller.
#'
#' @import
#' rgl
#'
#' @importFrom
#' Rvcg vcgPlyWrite
#'
#' @export
#' OPC3d
#'
#' @examples
#' OPC_output <- OPC(Tooth)
#' OPC3d(OPC_output)

OPC3d <- function (OPC_File, 
                   binColors = hsv(h=(seq(10, 290, 40)/360), s=0.9, v=0.85),
                   patchOutline = FALSE, outlineColor = "black", maskDiscard = FALSE,
                   legend = TRUE, main = '', cex = 1, scaleLegend = FALSE,
                   legendTextCol = "black", legendLineCol = "black", leftOffset = 1,
                   fieldofview = 0, fileName = NA, binary = FALSE)
{
  
  PD <- OPC_File$Patch_Details
	
	bins <- numeric(length=length(PD))
	
	param <- OPC_File$Parameters
	criteria <- which(param!=0)
	if(length(criteria)==1){
		criteria <- param[[2]]
		for (i in 1:length(PD)){
			legit <- which(PD[[i]][,1]>=criteria)
			bins[i] <- sum(PD[[i]][legit,2])
			names(bins) <- names(PD)
		}
	}
	if(length(criteria)==2){
		criteria <- param[[3]]
		for (i in 1:length(PD)){
			legit <- which(PD[[i]][,2]>=criteria)
			bins[i] <- sum(PD[[i]][legit,2])
			names(bins) <- names(PD)
		}
	}
  
 BinSizes <- bins[order(names(bins))]
    
  plyFile <- OPC_File$plyFile
  bins <- plyFile$Directional_Bins
  BinCount <- as.numeric(length(unique(plyFile$Directional_Bins)))
  BlackPatch <- NULL
  for (i in 1:BinCount) {
    Bin <- which(bins == i)
    bins[Bin] <- binColors[i]
    if (maskDiscard == TRUE) {
      if(OPC_File$Parameters$Minimum_Area==0){
        PatchList <- unlist(OPC_File$Patches[i], 
                            recursive = F)
        SmallPatch <- names(which(lapply(PatchList, length) < 
                                    OPC_File$Parameters$Minimum_Faces))
        Discarded <- as.numeric(unlist(PatchList[SmallPatch]))
        BlackPatch <- c(BlackPatch, Discarded)
      }
      if(OPC_File$Parameters$Minimum_Area>0){
        AreaList <- as.vector(OPC_File$Patch_Details[[i]][,2])
        MinAreaPercentage <- sum(OPC_File$plyFile$Face_Areas)*
          OPC_File$Parameters$Minimum_Area
        SmallPatchList <- which(AreaList < MinAreaPercentage)
        Discarded <- as.numeric(unlist(OPC_File$Patches[[i]][SmallPatchList]))
      }
      BlackPatch <- c(BlackPatch, Discarded)
    }
  }
  colormatrix <- bins
  if (maskDiscard == TRUE) {
    colormatrix[BlackPatch] <- "#000000"
  }
  open3d()
  layout3d(matrix(c(1,2), byrow=T, nrow=2), heights=c(1,9))
  par3d(windowRect = c(100, 100, 800, 800/.9))
  text3d(0,0,0, main, cex=cex*2.5, font=2)
  next3d()
  if (patchOutline == TRUE) {
    for (i in 1:BinCount) {
      Orientation <- OPC_File$Patches[i]
      PatchCount <- as.numeric(length(Orientation[[1]]))
      for (j in 1:PatchCount) {
        Patch <- Orientation[[1]][j]
        Patch <- as.numeric(Patch[[1]])
        Faces <- t(plyFile$it[, Patch])
        fnum <- length(Faces[, 1])
        vorder <- vector("list", fnum)
        for (i in 1:fnum) {vorder[[i]] <- unlist(sort(Faces[i, ]))}
        edges <- vector("list", fnum)
        for (i in 1:fnum) {
          Ordered <- vorder[[i]]
          G1 <- Ordered[1]
          G2 <- Ordered[2]
          G3 <- Ordered[3]
          ED1 <- paste(G1, G2, sep = "_")
          ED2 <- paste(G1, G3, sep = "_")
          ED3 <- paste(G2, G3, sep = "_")
          edges[[i]] <- paste(ED1, ED2, ED3, sep = ",")
        }
        for (i in 1:fnum) {edges[[i]] <- unlist(strsplit(edges[[i]], ","))}
        string <- unlist(edges)
        edgeframe <- data.frame(names = string)
        UniqueEdge <- aggregate(edgeframe, list(edgeframe$names), FUN = length)
        PatchEdge <- subset(UniqueEdge, UniqueEdge$names == 1)
        EdgeVerts <- as.numeric(unlist(strsplit(as.character(unlist(PatchEdge$Group.1)), "_")))
        EdgeCoords <- plyFile$vb[1:3, EdgeVerts]
        segments3d(t(EdgeCoords), color = outlineColor, 
                   lwd = 1.25, shininess = 120)
      }
    }
  }
  shade3d(plyFile, meshColor='faces', color = colormatrix, shininess = 110)
  view3d(fov = fieldofview)
  if (legend == TRUE) {
    if(cex <= 0){stop("cex must be a positive number")}
    if(cex > 1.25){
      warning("cex greater than 1.25 will restrict legend visibility")
    }
    Fills <- rep("#FFFFFF", BinCount)
    for (i in 1:BinCount) {
      Fills[i] <- binColors[i]
    }
    molaR_bgplot(OPC_Legend(binColors=Fills, binSize = BinSizes, scaleLegend = scaleLegend,
                            maskDiscard = maskDiscard, size = cex, textCol=legendTextCol,
                            lineCol=legendLineCol))
  }
  if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.055
  observer3d(XView, 0, ZView)
  if(!is.na(fileName)){
    if(!is.character(fileName)){stop("Enter a name for fileName")}
    if(substr(fileName, nchar(fileName)-3, nchar(fileName))!=".ply"){
      fileName <- paste(fileName, ".ply", sep="")
    }
    OutPly <- plyFile
    NewVertList <- plyFile$vb[,plyFile$it[1:length(plyFile$it)]]
    NewNormList <- plyFile$normals[,plyFile$it[1:length(plyFile$it)]]
    NewFaceList <- matrix(1:ncol(NewVertList), nrow=3)
    colormatrix <- matrix(rep(colormatrix, 3), nrow = 3, byrow = TRUE)
    NewColorList <- colormatrix[1:length(colormatrix)]
    OutPly$vb <- NewVertList
    OutPly$it <- NewFaceList
    OutPly$normals <- NewNormList
    OutPly$material$color <- NewColorList
    vcgPlyWrite(mesh=OutPly, filename = fileName, binary = binary)
    if(binary==FALSE){
      FileText <- readLines(con=paste(getwd(), "/", fileName, sep=""), warn = F)
      NewCom <- paste("comment OPC plot generated in molaR",
                      packageVersion("molaR"), "for", R.version.string)
      NewCom <- unlist(strsplit(NewCom, split='\n'))
      NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
      writeLines(NewOut, con=paste(getwd(), "/", fileName, sep=""))
    }
  }
}