#' Clip a ply file
#'
#' Function will clip a ply file along either the X, Y, or Z plane. The location for clipping 
#' can be indicated by the user through an interactive 3D window, or can be the index number 
#' of any vertex in the ply file.
#' 
#' @param plyFile An object of classes 'mesh3d' and 'shape3d'
#' @param axis Logical indicating the axis plane on which to clip the mesh. May be "X", "Y", 
#' or "Z". Defaults to "Z".
#' @param vertIndex Numeric index of a ply vertex to define clipping plane. See Details.
#' @param meshInvert Logical indicating whether or not to invert the mesh about the 
#' user-indicated axis. 
#' @param button Logical indicating which button on the mouse will select a region of the ply 
#' file. Must be one of 'right' (default), 'middle', or 'left'.
#' @param displayNew Logical indicating whether or not to display the ply file after clipping.
#' @param keepBoth Logical indicating whether or not to save both sides of the clipped ply.
#' @param edgeRefine Logical indicating whether or not to create a new, smooth edge along the 
#' indicated clipping plane.
#' 
#' @details This function returns a ply file that is clipped along a plane parallel to one of 
#' the three primary axes: X, Y, or Z. The location of the clipping plane is defined by one of 
#' the vertices in the ply file, and this 'focal vertex' is treated as a *minimum*. This means 
#' that regions of the ply file extending in a positive direction from the focal vertex along 
#' the user-defined axis (X, Y, or Z) will be retained as the user's selection, while regions 
#' of the negative to the focal vertex along the user-defined axis will be clipped out. The 
#' function offers two ways for the user to define the focal vertex: either by supplying an 
#' index number for a specific vertex in the ply file, or by allowing the user to interact 
#' with their ply in 3D and define a region of the ply in which to capture the focal vertex. 
#' It is assumed that most users will want to choose the region of their ply with the focal 
#' vertex, and the function therefore defaults to the interactive method unless a value is 
#' supplied for vertIndex.
#' 
#' When choosing the region with the focal vertex, a 3D interactive window displaying all ply 
#' vertices will appear. Users can manipulate this display with the left mouse button and 
#' zoom with the mouse wheel. The mouse button indicated by the button argument (defaults to 
#' 'right') allows the user to define a rectangular region of space in which to identify the 
#' focal vertex. The focal vertex is calculated as that vertex in the user-selected region 
#' with the minimum value along the axis indicated by the user in the axis argument (defaults 
#' to 'Z' axis). The function will identify the focal vertex in the user-defined region and 
#' allow users an opportunity to re-select their region before clipping. If users find that 
#' the function is retaining the opposite region of interest from the one they were interested 
#' in (i.e., if they wish to select the region of the ply file *negative* to the focal vertex 
#' along their specified axis), then the meshInvert argument should be altered to TRUE.
#' 
#' The keepBoth parameter allows users to retain both the positive and negative sides of their 
#' original ply file, now separated into two distinct plys. If this option is enabled, the 
#' function will return a list containing two objects: a 'Positive' ply file representing the 
#' region of the surface the function would have returned by default, and a 'Negative' ply 
#' file representing the remainder portion of the surface. This argument cannot currently be 
#' enabled with the edgeRefine argument is enabled.
#' 
#' The edgeRefine parameter is intended to produce a smooth edge along the plane identified 
#' by the user. By default the function will only retain those vertices and faces that were 
#' positive to the focal vertex along the axis specified by the user. This often produces an 
#' irregular, jagged surface edge along the boundaries of the retained faces. Enabling the 
#' edgeRefine argument smooths this edge out by adding new vertices and faces to fill in the 
#' jagged areas, creating a surface boundary even with the focal vertex. Implementing this 
#' step for a mesh with a large number of faces (>50,000) may be time-intensive. This argument 
#' cannot currently be enabled when the keepBoth argument is enabled. NOTE: ENABLING 
#' edgeRefine WILL MAKE THE OUTPUT UNSUITABLE FOR CALCULATING RFI.
#' 
#' This function can be used to clip ply files representing mammal tooth surfaces in such a 
#' way as to retain only the area of the tooth crown above the lowest point of the occlusal 
#' basin. This cropping procedure is consistent with the one used to prepare surfaces for 
#' measurement of RFI by Ungar and M'Kirera (2003).
#' 
#' It is recommended that users clean their ply files with the molaR_Clean() function prior 
#' to using this function, as unreferenced vertices can cause errors when recreating ply 
#' files. Future implementations of this function will allow the keepBoth and edgeRefine 
#' arguments to be enabled simultaneously and will allow users to define arbitrary planes 
#' (rather than those parallel to the primary axes) for clipping ply files.
#' 
#' @import
#' rgl
#'
#' @export
#' plyClip
#'

plyClip <- function(plyFile, axis = "Z", vertIndex = NA, meshInvert = FALSE, button="right",
                    displayNew = TRUE, keepBoth = FALSE, edgeRefine = FALSE)
{
  if(keepBoth==TRUE && edgeRefine==TRUE){
    stop("The keepBoth and edgeRefine arguments cannot be enabled at the same time.")
  }

  verts <- t(plyFile$vb[1:3,])
  oldEdge <- vcgGetEdge(plyFile)
  oldEdge <- oldEdge[oldEdge$border==1,]
  oldEdge <- sort(unique(c(oldEdge$vert1, oldEdge$vert2)))
  
  ##  Let users graphically choose location to crop mesh if no vertex input
  if(is.na(vertIndex)){
    if(button != "left" && button != "middle" && button != "right"){
      stop("Argument \'button\' should be one of \"left\", \"middle\", or \"right\"\n")
    }
    if(axis!="x" && axis!="X" && axis!="y" && axis!="Y" && axis!="z" && axis!="Z"){
      stop("Axis argument must be set to either X, Y, or Z")
    }
    
    open3d()
    points3d(verts)
    rgl.viewpoint(fov=0)
    if(meshInvert==TRUE){rgl.viewpoint(fov=0, theta=180)}
    cat("Use", button, "mouse button to select points from point cloud in 3D window.\n")
    selecter <- select3d(button=button)
    keep <- selecter(verts)
    if(sum(keep)==0){
      rgl.close()
      stop("No points selected!")
    }
    Keepers <- verts[keep,]
    if(rgl.cur()!=0){rgl.pop()}
    points3d(verts[keep,], size=3)
    
    ##  Determine which vertices will be kept and which will be lost based on axis designation
    if(axis=="x" || axis=="X"){
      if(meshInvert==FALSE){
        lowX <- min(Keepers[,1])
        lowPt <- which(Keepers[,1]==lowX)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,1]<lowX
      }
      if(meshInvert==TRUE){
        lowX <- max(Keepers[,1])
        lowPt <- which(Keepers[,1]==lowX)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,1]>lowX
      }
    }
    if(axis=="y" || axis=="Y"){
      if(meshInvert==FALSE){
        lowY <- min(Keepers[,2])
        lowPt <- which(Keepers[,2]==lowY)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,2]<lowY
      }
      if(meshInvert==TRUE){
        lowY <- max(Keepers[,2])
        lowPt <- which(Keepers[,2]==lowY)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,2]>lowY
      }
    }
    if(axis=="z" || axis=="Z"){
      if(meshInvert==FALSE){
        lowZ <- min(Keepers[,3])
        lowPt <- which(Keepers[,3]==lowZ)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,3]<lowZ
      }
      if(meshInvert==TRUE){
        lowZ <- max(Keepers[,3])
        lowPt <- which(Keepers[,3]==lowZ)
        lowPt <- Keepers[lowPt,]
        points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
        eliminate <- verts[,3]>lowZ
      }
    }
    if(length(eliminate)==0){stop(
      "Chosen vertex is the minimum for given surface & settings.")}
    points3d(verts[eliminate,], col="red", size=3)
    points3d(verts[!keep,], col="gray")
    rgl.viewpoint(fov=0)
    if(meshInvert==TRUE){rgl.viewpoint(fov=0, theta=180)}
    ActiveWin <- rgl.cur()[[1]]
    
    ##  Determine if user selection matches the user's desire
    cat("User-selected points appear in BLACK\n")
    message("Points to be eliminated appear in RED.\n")
    cat("Do you want to clip mesh? [Y/N]\n")
    answer <- readline(prompt="")
    if(answer!="Y" && answer!="y" && answer!="N" && answer!="n"){
      if(rgl.cur() == ActiveWin){rgl.close()}
      stop("Please enter \"Y\" or \"N\".\n")
    }
    if(answer=="N" || answer=="n"){
      if(rgl.cur() == ActiveWin){rgl.close()}
      plyClip(plyFile, axis = axis, vertIndex = vertIndex, meshInvert = meshInvert, button = button, displayNew = displayNew, keepBoth = keepBoth, edgeRefine = edgeRefine)
    }
  }
  
  if(!is.na(vertIndex)){
    ## Use vertex index input to automatically clip mesh based on X, Y, or Z plane
    if(!is.numeric(vertIndex) || !(vertIndex %in% c(1:nrow(verts)))){
      stop("vertIndex must be a numeric argument corresponding to a vertex index.\n
           Your mesh has ", nrow(verts), " vertices.")
    }
    if(axis=="x" || axis=="X"){
      lowX <- plyFile$vb[,vertIndex][[1]]
      if(meshInvert==FALSE){eliminate <- verts[,1]<lowX}
      if(meshInvert==TRUE){eliminate <- verts[,1]>lowX}
    }
    if(axis=="y" || axis=="Y"){
      lowY <- plyFile$vb[,vertIndex][[2]]
      if(meshInvert==FALSE){eliminate <- verts[,2]<lowY}
      if(meshInvert==TRUE){eliminate <- verts[,2]>lowY}
    }
    if(axis=="z" || axis=="Z"){
      lowZ <- plyFile$vb[,vertIndex][[3]]
      if(meshInvert==FALSE){eliminate <- verts[,3]<lowZ}
      if(meshInvert==TRUE){eliminate <- verts[,3]>lowZ}
    }
    if(length(eliminate)==0){stop(
      "Chosen vertex is the minimum for given surface & settings.")}
    answer <- "y"
    }

  ## If user is satisfied with selection, then clip mesh
  if(answer=="Y" || answer=="y"){
    if(rgl.cur() == ActiveWin){rgl.close()}
    
    ## Create the new mesh
    newPly <- plyFile
    plyVerts <- which(!eliminate)
    plyFaces <- matrix(plyFile$it %in% plyVerts, nrow=3)
    AllIn <- which(colSums(plyFaces)==3)
    newPly$it <- as.vector(plyFile$it[,AllIn])
    InFaces <- plyVerts %in% newPly$it
    NotIn <- sort(which(InFaces==FALSE))
    if(length(NotIn)>0){plyVerts <- plyVerts[-NotIn]}
    newPly$it <- matrix(as.numeric(factor(newPly$it)), nrow=3)
    newPly$vb <- plyFile$vb[,plyVerts]
    newPly$normals <- plyFile$normals[,plyVerts]
    
    ## If user has requested the edge to be refined, must create new vertices & faces
    if(edgeRefine==TRUE){
      newEdge <- vcgGetEdge(newPly)
      newEdge <- newEdge[newEdge$border==1,]
      
      ## 1. Remove any edges that are part of edge of original ply
      checkEdge <- sort(unique(c(newEdge$vert1, newEdge$vert2)))
      bouncedCheck <- which(plyVerts[checkEdge]%in%oldEdge)
      if(length(bouncedCheck)>0){
        i <- 1
        repeat{
          Bouncer1 <- plyVerts[newEdge[i,]$vert1]%in%oldEdge
          Bouncer2 <- plyVerts[newEdge[i,]$vert2]%in%oldEdge
          if(sum(Bouncer1, Bouncer2)==2){
            newEdge <- newEdge[-i,]
          }
          else{i <- i+1}
          if(i>nrow(newEdge)){break}
        }
      }
      
      ## 2. Check if any vertices were corners of new & old edge; use one as starting point
      edgeOccurrence <- as.data.frame(table(c(newEdge$vert1, newEdge$vert2)))
      cornerPts <- as.numeric(levels(edgeOccurrence[edgeOccurrence$Freq==1,]$Var1))[
        edgeOccurrence[edgeOccurrence$Freq==1,]$Var1]
      if(length(cornerPts)>0){
        Begin <- which(newEdge$vert1%in%cornerPts[1])
        vertCol <- 1
        if(sum(Begin)==0){
          Begin <- which(newEdge$vert2%in%cornerPts[1])
          vertCol <- 2
          if(Begin==1){
            Reverser <- newEdge[1,]
            newEdge[1,]$vert1 <- Reverser$vert2
            newEdge[1,]$vert2 <- Reverser$vert1
          }
        }
        if(vertCol==1 && Begin!=1){
          newEdge <- rbind(newEdge, newEdge[1,])
          newEdge[1,] <- newEdge[Begin,]
          newEdge <- newEdge[-Begin,]
        }
        if(vertCol==2 && Begin!=1){
          newEdge <- rbind(newEdge, newEdge[1,])
          newEdge[1,] <- newEdge[Begin,]
          newEdge <- newEdge[-Begin,]
          Reverser <- newEdge[1,]
          newEdge[1,]$vert1 <- Reverser$vert2
          newEdge[1,]$vert2 <- Reverser$vert1
        }
      }
      
      ## 3. Order the edges from one end to the next
      orderedEdge <- newEdge[1,]
      i <- 1
      j <- 0
      end <- nrow(newEdge)
      repeat{
        Vert2 <- orderedEdge[i,2]
        NextRow <- which(newEdge$vert1==Vert2)
        NextRow <- NextRow[length(NextRow)]
        if(length(NextRow)==1){
          orderedEdge <- rbind(orderedEdge, newEdge[NextRow,])
          newEdge <- newEdge[-NextRow,]
          i <- i+1
        }
        if(length(NextRow)==0){
          Reverser <- newEdge
          newEdge$vert1 <- Reverser$vert2
          newEdge$vert2 <- Reverser$vert1
          j <- j+1
        }
        if(j>i){
          Remainers <- as.data.frame(table(c(newEdge$vert1, newEdge$vert2)))
          Potentials <- as.numeric(as.character(Remainers$Var1[which(Remainers$Freq==1)]))
          InList <- which(Potentials %in% c(orderedEdge$vert1, orderedEdge$vert2))
          NewVert <- Potentials[-InList]
          CreateEdge <- cbind(orderedEdge[i,2], NewVert, 1, 1)
          colnames(CreateEdge) <- colnames(orderedEdge)
          orderedEdge <- rbind(orderedEdge, CreateEdge)
          i <- i+1
          j <- 0
          end <- end+1
        }
        if(i==end){break}
        if(j==end){break}
      }
      
      ## 4. Create new edge points based on the ordered edge vertices
      newEdgeVerts <- c(orderedEdge$vert1, orderedEdge$vert2)
      NotDuped <- which(!(duplicated(newEdgeVerts)))
      if(length(cornerPts)==0){
        edgePts <- matrix(nrow=3, ncol=length(NotDuped)*2)
      }
      if(length(cornerPts)>0){
        edgePts <- matrix(nrow=3, ncol=(length(NotDuped)*2)-1)
      }
      odds <- seq(1, ncol(edgePts), by=2)
      evens <- seq(2, ncol(edgePts), by=2)
      ## Use the user-defined X, Y, or Z plane to calculate new edge vertex locations
      if(axis=="x" || axis=="X"){
        edgePts[1,] <- lowPt[1]
        edgePts[,odds][2,] <- newPly$vb[,newEdgeVerts[NotDuped]][2,]
        edgePts[,odds][3,] <- newPly$vb[,newEdgeVerts[NotDuped]][3,]
        edgePts[,evens][2,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][2,], 
                                              newPly$vb[,orderedEdge[,2]][2,]))
        edgePts[,evens][3,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][3,], 
                                              newPly$vb[,orderedEdge[,2]][3,]))
      }
      if(axis=="y" || axis=="Y"){
        edgePts[2,] <- lowPt[2]
        edgePts[,odds][1,] <- newPly$vb[,newEdgeVerts[NotDuped]][1,]
        edgePts[,odds][3,] <- newPly$vb[,newEdgeVerts[NotDuped]][3,]
        edgePts[,evens][1,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][1,], 
                                              newPly$vb[,orderedEdge[,2]][1,]))
        edgePts[,evens][3,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][3,], 
                                              newPly$vb[,orderedEdge[,2]][3,]))
      }
      if(axis=="z" || axis=="Z"){
        edgePts[3,] <- lowPt[3]
        edgePts[,odds][1,] <- newPly$vb[,newEdgeVerts[NotDuped]][1,]
        edgePts[,odds][2,] <- newPly$vb[,newEdgeVerts[NotDuped]][2,]
        edgePts[,evens][1,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][1,], 
                                              newPly$vb[,orderedEdge[,2]][1,]))
        edgePts[,evens][2,] <- rowMeans(cbind(newPly$vb[,orderedEdge[,1]][2,], 
                                              newPly$vb[,orderedEdge[,2]][2,]))
      }
      edgePts <- rbind(edgePts, rep(1, ncol(edgePts)))
      
      ## 5. Determine face normal orientation and populate faces matrix
      IndexVert1 <- newEdgeVerts[NotDuped][1]
      IndexVert2 <- newEdgeVerts[NotDuped][2]
      IndexFaces <- matrix(newPly$it %in% IndexVert1, nrow=3)
      if(nrow(which(IndexFaces, arr.ind = T))==1){
        IndexFace <- which(matrix(newPly$it %in% IndexVert1, nrow=3), arr.ind = T)[2]
      }
      if(nrow(which(IndexFaces, arr.ind = T))>1){
        TargetFaces <- which(IndexFaces, arr.ind = T)[,2]
        IndexFace <- which(matrix(newPly$it[,TargetFaces] %in% IndexVert2, nrow=3),
                           arr.ind = T)[2]
        IndexFace <- TargetFaces[IndexFace]
      }
      IndexFace <- newPly$it[,IndexFace]
      if(length(cornerPts)==0){
        newFaces <- matrix(NA, nrow=3, ncol=1.5*ncol(edgePts))
      }
      if(length(cornerPts)>0){
        newFaces <- matrix(NA, nrow=3, ncol=(1.5*ncol(edgePts))-1)
      }
      firsts <- seq(1, ncol(newFaces), by=3)
      seconds <- seq(2, ncol(newFaces), by=3)
      thirds <- seq(3, ncol(newFaces), by=3)
      if(IndexVert1==IndexFace[1]&&IndexVert2==IndexFace[2] |
         IndexVert1==IndexFace[2]&&IndexVert2==IndexFace[3] |
         IndexVert1==IndexFace[3]&&IndexVert2==IndexFace[1]){
        for(i in 1:ncol(newFaces[,firsts])){
          newFaces[,firsts][1,i] <- newEdgeVerts[NotDuped[i]]
          newFaces[,firsts][2,i] <- ncol(newPly$vb)+odds[i]
          newFaces[,firsts][3,i] <- ncol(newPly$vb)+evens[i]
        }
        for(i in 1:ncol(newFaces[,seconds])){
          newFaces[,seconds][1,i] <- newEdgeVerts[NotDuped[i]]
          newFaces[,seconds][2,i] <- ncol(newPly$vb)+evens[i]
          newFaces[,seconds][3,i] <- newEdgeVerts[NotDuped[i+1]]
        }
        for(i in 1:ncol(newFaces[,thirds])){
          newFaces[,thirds][1,i] <- newEdgeVerts[NotDuped[i+1]]
          newFaces[,thirds][2,i] <- ncol(newPly$vb)+evens[i]
          newFaces[,thirds][3,i] <- ncol(newPly$vb)+odds[i+1]
        }
        if(length(cornerPts)==0){
          newFaces[3,max(seconds)] <- newEdgeVerts[NotDuped[1]]
          newFaces[1,max(thirds)] <- newEdgeVerts[NotDuped[1]]
          newFaces[3,max(thirds)] <- ncol(newPly$vb)+1
        }
      }
      if(IndexVert1==IndexFace[2]&&IndexVert2==IndexFace[1] |
         IndexVert1==IndexFace[3]&&IndexVert2==IndexFace[2] |
         IndexVert1==IndexFace[1]&&IndexVert2==IndexFace[3]){
        for(i in 1:ncol(newFaces[,firsts])){
          newFaces[,firsts][1,i] <- newEdgeVerts[NotDuped[i]]
          newFaces[,firsts][2,i] <- ncol(newPly$vb)+evens[i]
          newFaces[,firsts][3,i] <- ncol(newPly$vb)+odds[i]
        }
        for(i in 1:ncol(newFaces[,seconds])){
          newFaces[,seconds][1,i] <- newEdgeVerts[NotDuped[i]]
          newFaces[,seconds][2,i] <- newEdgeVerts[NotDuped[i+1]]
          newFaces[,seconds][3,i] <- ncol(newPly$vb)+evens[i]
        }
        for(i in 1:ncol(newFaces[,thirds])){
          newFaces[,thirds][1,i] <- newEdgeVerts[NotDuped[i+1]]
          newFaces[,thirds][2,i] <- ncol(newPly$vb)+odds[i+1]
          newFaces[,thirds][3,i] <- ncol(newPly$vb)+evens[i]
        }
        if(length(cornerPts)==0){
          newFaces[2,max(seconds)] <- newEdgeVerts[NotDuped[1]]
          newFaces[1,max(thirds)] <- newEdgeVerts[NotDuped[1]]
          newFaces[2,max(thirds)] <- ncol(newPly$vb)+1
        }
      }
      
      ## 6. Create normals
      newNorms <- matrix(nrow=4, ncol=ncol(edgePts))
      newNorms[,odds] <- newPly$normals[,newEdgeVerts[NotDuped]]
      normSeq <- sort(rep(1:length(NotDuped), 2))
      normSeq <- normSeq[-1]
      if(length(cornerPts)==0){
        normSeq <- c(normSeq, 1)
        for(i in 1:end-1){
          pairing <- cbind(newNorms[,odds][,i], newNorms[,odds][,i+1])
          newNorms[,evens][,i] <- rowMeans(pairing)
        }
        newNorms[,evens][,end] <- rowMeans(cbind(newNorms[,odds][,end], newNorms[,1]))
      }
      if(length(cornerPts)>0){
        for(i in 1:end){
          pairing <- cbind(newNorms[,odds][,i], newNorms[,odds][,i+1])
          newNorms[,evens][,i] <- rowMeans(pairing)
        }
      }
      
      ## 7. Add refined edge faces, normals, and vertices into new ply file
      newPly$it <- cbind(newPly$it, newFaces)
      newPly$vb <- cbind(newPly$vb, edgePts)
      newPly$normals <- cbind(newPly$normals, newNorms)
    }
    
    ## If user requests keeping both sides of file
    if(keepBoth==TRUE){
      otherPly <- plyFile
      newEdge <- vcgGetEdge(newPly)
      newEdge <- newEdge[newEdge$border==1,]
      newEdge <- plyVerts[unique(c(newEdge$vert1, newEdge$vert2))]
      #The following line still misses corner points
      newerEdge <- newEdge[which(!(newEdge %in% oldEdge))]
      otherVerts <- sort(unique(c(which(eliminate), newerEdge)))
      otherFaces <- matrix(plyFile$it %in% otherVerts, nrow=3)
      AllIn2 <- which(colSums(otherFaces)==3)
      otherPly$it <- as.vector(plyFile$it[,AllIn2])
      InFaces2 <- otherVerts %in% otherPly$it
      NotIn2 <- sort(which(InFaces2==FALSE))
      if(length(NotIn2)>0){otherVerts <- otherVerts[-NotIn2]}
      otherPly$it <- matrix(as.numeric(factor(otherPly$it)), nrow=3)
      otherPly$vb <- plyFile$vb[,otherVerts]
      otherPly$normals <- plyFile$normals[,otherVerts]
    }
    
    ## Final steps
    if(displayNew==TRUE){
      open3d()
      if(keepBoth==FALSE){
        shade3d(newPly, col="gray")
        points3d(t(plyFile$vb[,eliminate]), col="red", size=3)
      }
      if(keepBoth==TRUE){
        shade3d(newPly, col="gray")
        shade3d(otherPly, col="#404040")
      }
      rgl.viewpoint(fov=0)
      if(meshInvert==TRUE){rgl.viewpoint(fov=0, theta=180)}
    }
    invisible(capture.output(newPly <- molaR_Clean(newPly)))
    if(keepBoth==FALSE){
      return(newPly)
    }
    if(keepBoth==TRUE){
      invisible(capture.output(otherPly <- molaR_Clean(otherPly)))
      plyList <- list(Positive = newPly, Negative = otherPly)
      return(plyList)
    }
  }
}