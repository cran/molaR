# Internal function for separating mesh into two meshes at specified plane

meshClip <- function(plyFile, A, B, C, D){
  verts <- t(plyFile$vb[1:3,])
  faces <- t(plyFile$it)
  ## Determine if vertices are above, in, or below plane
  vertDist <- apply(verts, 1, function(x){(A*x[1] + B*x[2] + C*x[3] + D)/sqrt(sum(A^2, B^2, C^2))})
  vPos <- as.numeric(vertDist >= 0)
  ## Describe faces as binary value based on condition
  fPos <- matrix(as.character(vPos[faces]), ncol = 3)
  fBin <- strtoi(apply(fPos, 1, function(x){paste0(x, collapse = "")}), base = 2)
  if(sum(fBin == 7) == nrow(faces)){
    meshA <- plyFile
    meshB <- NULL
    out <- list("meshA" = meshA, "meshB" = meshB)
    return(out)
    stop(call. = FALSE, domain = NA)
  }
  if(sum(fBin == 0) == nrow(faces)){
    meshA <- NULL
    meshB <- plyFile
    out <- list("meshA" = meshA, "meshB" = meshB)
    return(out)
    stop(call. = FALSE, domain = NA)
  }
  ## Determine which mesh edges cross the plane, find points of intersection
  all.Edges <- rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  X.edges <- which(vPos[all.Edges[,1]]+vPos[all.Edges[,2]] == 1)
  X.edges <- all.Edges[X.edges,]
  Pl.point <- c(A, B, C) * -D/sum(A^2, B^2, C^2)
  Dupes <- which(duplicated(lapply(1:nrow(X.edges), function(y){Q <- X.edges[y,]; Q[order(Q)]})))
  X.unique <- X.edges[-Dupes,]
  vec1 <- verts[X.unique[,2],]-verts[X.unique[,1],]
  vec2 <- sweep(verts[X.unique[,1],], 2, Pl.point, check.margin = FALSE)
  M <- apply(vec1, 1, function(x) c(A, B, C) %*% x)
  N <- apply(vec2, 1, function(x) -(c(A, B, C) %*% x))
  I <- N/M
  newVerts <- verts[X.unique[,1],] + (I*vec1)
  newNormals <- vcgClost(newVerts, plyFile)[[2]]
  allVerts <- rbind(verts, newVerts)
  allNorms <- cbind(plyFile$normals, newNormals)
  ## Determine handling for each face based on 8 plane-crossing conditions
  vert.Tab <- c(apply(X.unique, 1, function(x){paste0(x, collapse = "")}),
                    apply(cbind(X.unique[,2], X.unique[,1]), 1, function(x){paste0(x, collapse = "")}))
  vert.Tab <- data.frame("Combo" = vert.Tab, "Index" = c(rep(1:nrow(newVerts), 2)))
  A.faces <- faces[which(fBin==7),]
  B.faces <- faces[which(fBin==0),]
  A.rows <- length(c(which(fBin == 1), which(fBin == 2), which(fBin == 4))) + 2*length(c(which(fBin == 3), which(fBin == 5), which(fBin == 6)))
  B.rows <- 2*length(c(which(fBin == 1), which(fBin == 2), which(fBin == 4))) + length(c(which(fBin == 3), which(fBin == 5), which(fBin == 6)))
  A.More <- matrix(nrow = A.rows, ncol = 3)
  B.More <- matrix(nrow = B.rows, ncol = 3)
  A.row <- 1
  B.row <- 1
  for(i in which(fBin==1)){  # A has v3; B has v1 and v2
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 1, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.row <- A.row + 1
    B.More[B.row,] <- newTris[2,]
    B.More[B.row + 1,] <- newTris[3,]
    B.row <- B.row + 2
  }
  for(i in which(fBin==2)){  # A has v2; B has v1 and v3
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 2, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.row <- A.row + 1
    B.More[B.row,] <- newTris[2,]
    B.More[B.row + 1,] <- newTris[3,]
    B.row <- B.row + 2
  }
  for(i in which(fBin==3)){  # A has v2 and v3; B has v1
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 3, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.More[A.row + 1,] <- newTris[2,]
    A.row <- A.row + 2
    B.More[B.row,] <- newTris[3,]
    B.row <- B.row + 1
  }
  for(i in which(fBin==4)){  # A has v1; B has v2 and v3
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 4, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.row <- A.row + 1
    B.More[B.row,] <- newTris[2,]
    B.More[B.row + 1,] <- newTris[3,]
    B.row <- B.row + 2
  }
  for(i in which(fBin==5)){  # A has v1 and v3; B has v2
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 5, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.More[A.row + 1,] <- newTris[2,]
    A.row <- A.row + 2
    B.More[B.row,] <- newTris[3,]
    B.row <- B.row + 1
  }
  for(i in which(fBin==6)){  # A has v1 and v2; B has v3
    curFace <- faces[i,]
    newTris <- triCondition(curFace, vert.Tab, 6, nrow(verts))
    A.More[A.row,] <- newTris[1,]
    A.More[A.row + 1,] <- newTris[2,]
    A.row <- A.row + 2
    B.More[B.row,] <- newTris[3,]
    B.row <- B.row + 1
  }
  A.faces <- rbind(A.faces, A.More)
  B.faces <- rbind(B.faces, B.More)
  ## Clean up vertex and face indexing
  allVerts <- cbind(allVerts, c(1:nrow(allVerts)))
  A.vb <- allVerts[sort(unique(as.vector(A.faces))),]
  A.normals <- allNorms[,sort(unique(as.vector(A.faces)))]
  B.vb <- allVerts[sort(unique(as.vector(B.faces))),]
  B.normals <- allNorms[,sort(unique(as.vector(B.faces)))]
  A.it <- apply(A.faces, c(1, 2), function(x){which(A.vb[, 4] == x)})
  B.it <- apply(B.faces, c(1, 2), function(x){which(B.vb[, 4] == x)})
  A.vb[,4] <- 1; B.vb[,4] <- 1
  meshA <- plyFile
  meshA$vb <- t(A.vb)
  meshA$it <- t(A.it)
  meshA$normals <- A.normals
  meshA <- molaR_Clean(meshA, verbose = FALSE)
  meshB <- plyFile
  meshB$vb <- t(B.vb)
  meshB$it <- t(B.it)
  meshB$normals <- B.normals
  meshB <- molaR_Clean(meshB, verbose = FALSE)
  out <- list("meshA" = meshA, "meshB" = meshB)
  return(out)
}
