#' Plot advanced results of a DNE surface analysis 
#' 
#'
#' a molaR surface plotting function
#' 
#' @param DNE_File An object that stores the output of the `DNE()`
#' function
#' @param baseCol Base color for typical face on surface. Default is gray
#' @param boundCol Color for the boundary faces discarded from 
#' the DNE calculation. Default is red.
#' @param outlierCol Color for the faces discarded as outliers from
#'  the DNE calculation. Default is lawngreen
#' @param main string indicating plot title. Defaults to empty
#' @param cex Defaults to 1 sets the size of the title for the plot
#' @param concaveCol Color of the Concave faces on the surface. 
#' When left in default concave faces remain undistinguished on
#' the plotted surface and are colored the same as baseCol.
#' @param cex numeric value setting the relative size of the legend, default=1
#' @param cex.main numeric value setting the relative size of the plot title,
# default=2.5
#' @param legend Logical indicating whether or not a legend
#' shold be displayed. Default=T
#' @param leftOffset numeric value between -1 and 1 setting the degree of
#' offset for the plotted surface to the left. Larger values set further to right. 
#' Default=1
#' @param fieldofview Passes an argument to `par3d()` changing the field of
#' view (in degrees) of the resulting 3D plot
#' @param fileName String indicating a name to save the plotted surface to as a
#' *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should
#' be binary, passed to `vcgPlyWrite()`
#'
#' @details This function creates a surface map of the discarded DNE faces. 
#' DNE calculations typically discard the top 1 tenth of one percent of faces, 
#' associated with extreme pockets and broken parts of surfaces. DNE 
#' calculations also typically discard the boundary faces from the calculation, 
#' either on the basis of 2 vertices on the boundary, or at least one vertext on 
#' the boundary. concaveCol defaults to gray and therefore is turned off. When
#' an alternative color is provided, the function will identify the the areas of the 
#' tooth that are concave vs convex. 
#' 
#' Details of the other function arguments can be found in the DNE3d() description
#' and identical terms are organized to function the same way. 
#' 
#'
#'
#' @import
#' rgl grDevices graphics utils
#' 
#' @export
#' DNE3dDiscard
#'
#' @examples
#' DNE_output <- DNE(Tooth)
#' DNE3dDiscard(DNE_output)

DNE3dDiscard <- function (DNE_File, baseCol='gray', boundCol='red', outlierCol='lawngreen', concaveCol=baseCol, main='', cex=1,  cex.main=2.5, legend=T, leftOffset=1, fieldofview=0, fileName=NA, binary = FALSE) 
{
  plyFile <- DNE_File$plyFile
  k <- DNE_File$Kappa
  DNE_colors <- rep(baseCol, length=length(DNE_File$Face_Values$Face_Areas))
  
  	inflection <- which(DNE_File$Face_Values$Kappa_Values<=k)
    DNE_colors[inflection] <- concaveCol
    edges <- as.numeric(rownames(DNE_File$Boundary_Values))
    DNE_colors[edges] <- boundCol
    outliers <- as.numeric(rownames(DNE_File$Outliers))
    DNE_colors[outliers] <- outlierCol
    
    
    
  open3d()

  layout3d(matrix(c(1,2), byrow=T, nrow=2), heights=c(1,9))
  par3d(windowRect = c(100, 100, 800, 800/0.9))

text3d(0,0,0, main, cex=cex.main, font=2);next3d()  
 
  shade3d(plyFile, color = DNE_colors, meshColor='faces', shininess = 110)
  if (legend == TRUE) {
    if(cex <= 0){stop("cex must be a positive number")}
    if(cex > 1.25){
      warning("cex greater than 1.25 will restrict legend visibility")
    }
    Base <- baseCol
    Concave <- concaveCol
    Bound <- boundCol
    Outlier <- outlierCol
    scaled <- F
    molaR_bgplot(DNE_Discard_Legend(Base, Bound, Outlier, Concave, size=cex))
  }
  if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  view3d(fov = fieldofview)
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.05
  observer3d(XView, 0, ZView)
  
    if (!is.na(fileName)) {
    if (!is.character(fileName)) {stop("Enter a name for fileName")}
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply") {
      fileName <- paste(fileName, ".ply", sep = "")
    }
    plyFile$material$color <- DNE_colors
    vcgPlyWrite(mesh = plyFile, filename = fileName, binary = binary)
    if (binary == FALSE) {
      FileText <- readLines(con = paste(getwd(), "/", 
                                        fileName, sep = ""), warn = F)
      NewCom <- paste("comment DNE plot generated in molaR", 
                      packageVersion("molaR"), "for", R.version.string)
      NewCom <- unlist(strsplit(NewCom, split = "\n"))
      NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
      writeLines(NewOut, con = paste(getwd(), "/", 
                                     fileName, sep = ""))
    }
  }
}