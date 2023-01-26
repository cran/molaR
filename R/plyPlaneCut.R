#' Cut a PLY Mesh Along a Specified Plane
#'
#' `plyPlaneCut` permits several different approaches for specifying a cutting plane 
#' and returns either a portion of the original mesh from one side of the plane, or 
#' both portions from each side of the plane stored as separate list elements.
#' 
#' @param plyFile An object of class 'mesh3d'.
#' @param axis String indicating the axis plane on which to cut the mesh. May be `'X'`, `'Y'`, 
#' or `'Z'`, Defaults to `'Z'`. Ignored if `plane` is specified, see details.
#' @param vertIndex Numeric index of a mesh vertex to define clipping plane. Ignored 
#' if `plane` is specified, see details.
#' @param keepBoth Logical indicating if both sides of the cut mesh should be returned, 
#' defaults to `FALSE`. If `TRUE` (and the cutting plane intersects the mesh), the function 
#' output is a list containing `meshA` and `meshB` representing the two portions.
#' @param plane Requires four numeric values specifying the coordinates of the plane normal 
#' (*a, b, c*) and the "offset" (*d*). Overrides input for `axis` and `vertIndex`, see details.
#' @param col Vector indicating the color for vertex drawing when interactively choosing
#' cutting plane. Defaults to `"rainbow"`, a magenta-to-red color ramp along the specified 
#' `axis`.
#' @param flipAxis Logical indicating whether or not to reverse the output about the 
#' normal of the plane, defaults to `FALSE`. 
#' @param displayNew Logical indicating whether or not to display the function results 
#' when a value is supplied to either `vertIndex` or `plane`, defaults to `TRUE`.
#' 
#' @details `plyPlaneCut` draws a cutting plane using the parametrization *ax + by + cz + d = 0* 
#' (Hesse normal form), wherein <*a, b, c*> constitute the normal to the plane, and *d* is the 
#' "offset" value. See \code{\link[rgl]{planes3d}} for further information. Users can supply 
#' any parameters for `a`, `b`, `c`, and `d` in the `plane` argument to produce an arbitrary 
#' cutting plane (see Examples), however the function is designed to aid users in choosing a 
#' cutting plane without foreknowledge of the desired parameters.
#' 
#' When `plane` is `NA`, the function will cut the mesh along a plane orthogonal to one of 
#' the primary axes (X, Y, or Z, as indicated by `axis`) at the location of a focal vertex. 
#' The focal vertex can be defined by its index value, supplied to `vertIndex`. If no value 
#' is given for either `plane` or `vertIndex`, then an interactive 3D window allows the 
#' user to select the focal vertex. A 3D window will open displaying all mesh vertices, 
#' colored according to `col`, with a semi-transparent mesh surface. The display can be 
#' rotated with the left mouse button and zoomed with the mouse wheel. The right mouse 
#' button allows the user to define a rectangular region in which to identify the focal 
#' vertex. The focal vertex is the vertex in the user-selected region with the *minimum value* 
#' in the dimension indicated by the `axis` argument. A preview of the resulting cutting 
#' will be supplied, and for the function to finish users must supply a "Y" or "y" 
#' confirmation to the `Cut mesh?:` prompt in the terminal. Any other response will 
#' restart the selection process.
#' 
#' The `col` argument is only invoked when choosing a focal vertex in an interactive 3D 
#' window (i.e., `vertIndex` and `plane` are set to `NA`). This argument will apply any 
#' acceptable color vector to the displayed vertices. Alternatively, users can specify 
#' a color ramp by supplying a string, including: `"rainbow"`, `"heat.colors"`, 
#' `"terrain.colors"`, `"topo.colors"`, `"cm.colors"`, or `"gray.colors"`; see 
#' \code{\link[grDevices]{hcl.colors}} and \code{\link[grDevices]{gray.colors}} for 
#' further details. Color ramps will plot along the axis specified by `axis` and reverse 
#' if `flipAxis = TRUE`.
#' 
#' If users prefer that the function is inverted with respect to mesh geometry (i.e., that it 
#' identifies the focal vertex as the *maximum value* with respect to `axis`, or that the 
#' resulting mesh be that along the *negative* normal to the plane), then they should set 
#' `flipAxis = TRUE`. If `keepBoth` is enabled, the function will return a list of two 
#' 'mesh3d' objects: `meshA`, and `meshB`. Enabling `keepBoth` but providing a plane 
#' that does not intersect the mesh will result in a list with one of the objects set 
#' to `NULL` (see Examples).
#' 
#' This function can be used to cut meshes representing tooth surfaces so as to retain 
#' only the area of the tooth crown above the lowest point of the occlusal basin. This 
#' cropping procedure is consistent with the one used to prepare surfaces for 
#' measurement of occlusal relief (OR) by Ungar & M'Kirera (2003) "A solution to the 
#' worn tooth conundrum in primate functional anatomy" PNAS 100(7):3874-3877 
#' Unreferenced vertices can cause errors, so users are encouraged to clean their mesh 
#' with \code{\link{molaR_Clean}} prior to using this function.
#' 
#' @return 
#' An object of class 'mesh3d' corresponding to the portion of the mesh on one side 
#' of the cutting plane. If `keepBoth` is enabled, a list of two such objects corresponding 
#' to the portions from both sides of the plane.
#' 
#' @importFrom 
#' Rvcg vcgClost
#' 
#' @import
#' rgl
#'
#' @export
#' plyPlaneCut
#' 
#' @examples 
#' # Result from providing plane parameters and keeping meshes from both sides of plane
#' 
#' cutMesh <- plyPlaneCut(Tooth, plane = c(0.5, 0.5, 0.5, -4), keepBoth = TRUE)
#' open3d()
#' shade3d(cutMesh$meshA, col = "gray")
#' wire3d(cutMesh$meshB)
#' planes3d(0.5, 0.5, 0.5, -4, col = "red", alpha = 0.66)
#' 
#' 
#' # Result from providing parameters for a plane that does not intersect the mesh
#' 
#' cutMesh <- plyPlaneCut(Tooth, plane = c(1, 0.75, 0.5, -11))
#' identical(Tooth, cutMesh)
#' 
#' cutMesh <- plyPlaneCut(Tooth, plane = c(1, 0.75, 0.5, -11), keepBoth = TRUE)
#' identical(Tooth, cutMesh)
#'

plyPlaneCut <- function(plyFile, axis = "Z", vertIndex = NA, keepBoth = FALSE, 
                           plane = NA, col = "rainbow", flipAxis = FALSE, displayNew = TRUE){
  answer <- "Y"
  verts <- t(plyFile$vb[1:3,])
  faces <- t(plyFile$it)
  cent <- c(mean(plyFile$vb[1,]), mean(plyFile$vb[2,]), mean(plyFile$vb[3,]))
  if(length(plane)==1 && is.na(plane)){
    cropDim <- rep(c(1, 2, 3), 2)[which(c(letters[24:26], LETTERS[24:26]) %in% axis)]
    if(length(cropDim)==0){stop("Axis argument must be set to either X, Y, or Z")}
  }
  ## If no vertex index or plane pre-defined, then user manually chooses clipping location
  if(is.na(vertIndex) && length(plane)==1 && is.na(plane)){answer <- "N"}
  while(answer == "N"){
    vertCol <- col
    if(length(vertCol) == 1){
      if(col == "gray.colors"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- gray.colors(nrow(verts), start = 0.1, rev = TRUE)}
        else{vertCol[order(verts[,cropDim])] <- gray.colors(nrow(verts), start = 0.1)}
      }
      if(col == "rainbow"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- rainbow(nrow(verts), end = 0.9)}
        else{vertCol[order(verts[,cropDim])] <- rainbow(nrow(verts), end = 0.9, rev = TRUE)}
      }
      if(col == "heat.colors"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- heat.colors(nrow(verts), rev = TRUE)}
        else{vertCol[order(verts[,cropDim])] <- heat.colors(nrow(verts))}
      }
      if(col == "terrain.colors"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- terrain.colors(nrow(verts), rev = TRUE)}
        else{vertCol[order(verts[,cropDim])] <- terrain.colors(nrow(verts))}
      }
      if(col == "topo.colors"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- topo.colors(nrow(verts), rev = TRUE)}
        else{vertCol[order(verts[,cropDim])] <- topo.colors(nrow(verts))}
      }
      if(col == "cm.colors"){
        if(flipAxis == TRUE){vertCol[order(verts[,cropDim])] <- cm.colors(nrow(verts), rev = TRUE)}
        else{vertCol[order(verts[,cropDim])] <- cm.colors(nrow(verts))}
      }
    }
    open3d()
    points3d(verts, col = vertCol, size = 3)
    shade3d(plyFile, col = "#FFFFFF", alpha = 0.5)
    view3d(fov=0)
    cat("Click and drag right mouse button to select points in 3D window.\n")
    selecter <- select3d(button = "right")
    keep <- selecter(verts)
    if(sum(keep)==0){rgl.close(); stop("No points selected!")}
    planeNorm <- c(0, 0, 0)
    if(flipAxis == FALSE){
      lowPt <- which(verts[keep, cropDim] == min(verts[keep, cropDim]))
      lowPt <- verts[which(keep)[lowPt],]
      planeNorm[cropDim] <- 1
      offset <- lowPt[cropDim]
    }
    if(flipAxis == TRUE){
      lowPt <- which(verts[keep, cropDim] == max(verts[keep, cropDim]))
      lowPt <- verts[which(keep)[lowPt],]
      planeNorm[cropDim] <- -1
      offset <- lowPt[cropDim]*-1
    }
    newMesh <- meshClip(plyFile, planeNorm[1], planeNorm[2], planeNorm[3], -offset)
    rgl.clear()
    clipDisplay(newMesh, c(planeNorm, -offset), cent, keepers = verts[keep,], focal = lowPt, keepBoth)
    ActiveWin <- rgl.cur()[[1]]
    cat("User-selected points appear in black; solid mesh(es) will be retained.
        Enter \'Y\' to cut mesh; all other input will restart operation.\n")
    response <- readline(prompt = "Cut mesh?: ")
    if(rgl.cur() == ActiveWin){rgl.close()}
    if(response == "Y" || response == "y"){answer <- "Y"}
  }
  ## If vertIndex is defined, clip to that location according to defined axis
  if(!is.na(vertIndex) && (length(plane)==1 && is.na(plane))){
    if(vertIndex < 1 || vertIndex > ncol(plyFile$vb)){
      stop("Invalid vertex index.")
    }
    planeNorm <- c(0, 0, 0)
    lowPt <- verts[vertIndex,]
    if(flipAxis == FALSE){
      planeNorm[cropDim] <- 1
      offset <- lowPt[cropDim]
    }
    if(flipAxis == TRUE){
      planeNorm[cropDim] <- -1
      offset <- lowPt[cropDim]*-1
    }
    newMesh <- meshClip(plyFile, planeNorm[1], planeNorm[2], planeNorm[3], -offset)
    if(displayNew == TRUE){
      view3d(fov=0)
      clipDisplay(newMesh, c(planeNorm, -offset), cent, keepers = NA, focal = lowPt, keepBoth)
    }
  }
  ## if plane is defined, ignore other user input and simply populate meshClip
  if(!(length(plane)==1 && is.na(plane))){
    if(!is.numeric(plane) || length(plane)!=4){
      stop("Plane must be defined using Hesse normal form, with parameterization ax + by + cz + d = 0
           Please supply numeric values for a, b, c, and d.")
    }
    if(flipAxis == TRUE){plane[1:3] <- -plane[1:3]}
    newMesh <- meshClip(plyFile, plane[1], plane[2], plane[3], plane[4])
    if(displayNew == TRUE){
      clipDisplay(newMesh, plane = c(plane[1], plane[2], plane[3], -plane[4]), 
                  cent, keepers = NA, focal = NA, keepBoth)
      view3d(fov=0)
    }
  }
  if(is.null(newMesh$meshA)){warning("Entire mesh is below plane")}
  if(is.null(newMesh$meshB)){warning("Entire mesh is above plane")}
  if(keepBoth == FALSE){
    if(is.null(newMesh[[1]])){out <- newMesh[[2]]}
    else{out <- newMesh[[1]]}
  }
  if(keepBoth == TRUE){out <- newMesh}
  return(out)
}
