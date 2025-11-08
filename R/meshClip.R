#' Split a mesh by a plane into two meshes
#'
#' Internal utility that clips an rgl mesh3d by a plane defined as
#' A*x + B*y + C*z + D = 0, returning the two resulting meshes.
#'
#' @param plyFile An rgl \code{mesh3d} object to be clipped.
#' @param A,B,C,D Numeric coefficients of the clipping plane
#'   (plane equation: \code{A*x + B*y + C*z + D = 0}).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{meshA}}{The submesh on the side where \code{A*x + B*y + C*z + D >= 0}.}
#'   \item{\code{meshB}}{The complementary submesh (may be \code{NULL} if empty).}
#' }
#'
#' @keywords internal
#' @importFrom Rvcg vcgClost
meshClip <- function(plyFile, A, B, C, D) {
  # ---- rgl device setup (headless): minimal, scoped, auto-restore -------------
  old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)

  # vertices (n x 3) and faces (nF x 3) as row-wise matrices
  verts <- t(plyFile$vb[1:3, ])
  faces <- t(plyFile$it)

  # --- vertex-side test relative to plane -------------------------------------
  # signed distance to plane (normalized by plane normal length)
  vertDist <- apply(
    verts, 1,
    function(x) {
      (A * x[1] + B * x[2] + C * x[3] + D) / sqrt(sum(A^2, B^2, C^2))
    }
  )
  vPos <- as.numeric(vertDist >= 0)

  # --- encode each face as a 3-bit binary (0/1 per vertex) and to decimal -----
  fPos <- matrix(as.character(vPos[faces]), ncol = 3)
  fBin <- strtoi(apply(fPos, 1, function(x) paste0(x, collapse = "")), base = 2)

  # all faces on A side
  if (sum(fBin == 7) == nrow(faces)) {
    meshA <- plyFile
    meshB <- NULL
    out <- list("meshA" = meshA, "meshB" = meshB)
    return(out)
  }
  # all faces on B side
  if (sum(fBin == 0) == nrow(faces)) {
    meshA <- NULL
    meshB <- plyFile
    out <- list("meshA" = meshA, "meshB" = meshB)
    return(out)
  }

  # --- find mesh edges crossing the plane and intersection points --------------
  all.Edges <- rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  X.edges <- which(vPos[all.Edges[, 1]] + vPos[all.Edges[, 2]] == 1)
  X.edges <- all.Edges[X.edges, , drop = FALSE]

  # plane point (projection of origin onto plane): n * (-D / ||n||^2)
  Pl.point <- c(A, B, C) * -D / sum(A^2, B^2, C^2)

  # deduplicate undirected edges
  Dupes <- which(duplicated(lapply(1:nrow(X.edges), function(y) {
    Q <- X.edges[y, ]
    Q[order(Q)]
  })))
  X.unique <- X.edges[-Dupes, , drop = FALSE]

  # intersection parameter for each crossing edge
  vec1 <- verts[X.unique[, 2], ] - verts[X.unique[, 1], ]
  vec2 <- sweep(verts[X.unique[, 1], ], 2, Pl.point, check.margin = FALSE)
  M <- apply(vec1, 1, function(x) c(A, B, C) %*% x)
  N <- apply(vec2, 1, function(x) -(c(A, B, C) %*% x))
  I <- as.numeric(N / M)

  # new intersection vertices along those edges
  newVerts <- verts[X.unique[, 1], ] + (I * vec1)

  # --- gather normals at intersection points (namespaced for R CMD check) ------
  cl <- Rvcg::vcgClost(newVerts, plyFile)
  # cl$normals is k x 3 (per queried point); transpose to 3 x k like rgl normals
  newNormals <- t(cl$normals)

  allVerts <- rbind(verts, newVerts)
  allNorms <- cbind(plyFile$normals, newNormals)

  # --- route faces to A or B and triangulate across the cut --------------------
  # lookup table to map oriented edge "ij" -> intersection index
  vert.Tab <- c(
    apply(X.unique, 1, function(x) paste0(x, collapse = "")),
    apply(cbind(X.unique[, 2], X.unique[, 1]), 1, function(x) paste0(x, collapse = ""))
  )
  vert.Tab <- data.frame("Combo" = vert.Tab, "Index" = c(rep(1:nrow(newVerts), 2)))

  A.faces <- faces[which(fBin == 7), , drop = FALSE]
  B.faces <- faces[which(fBin == 0), , drop = FALSE]

  A.rows <- length(c(which(fBin == 1), which(fBin == 2), which(fBin == 4))) +
    2 * length(c(which(fBin == 3), which(fBin == 5), which(fBin == 6)))
  B.rows <- 2 * length(c(which(fBin == 1), which(fBin == 2), which(fBin == 4))) +
    length(c(which(fBin == 3), which(fBin == 5), which(fBin == 6)))

  A.More <- matrix(nrow = A.rows, ncol = 3)
  B.More <- matrix(nrow = B.rows, ncol = 3)
  A.row <- 1
  B.row <- 1

  # helper tri splitting for the 6 mixed cases; relies on existing internal triCondition()
  for (i in which(fBin == 1)) { # A has v3; B has v1 and v2
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 1, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.row <- A.row + 1
    B.More[B.row, ] <- newTris[2, ]
    B.More[B.row + 1, ] <- newTris[3, ]
    B.row <- B.row + 2
  }
  for (i in which(fBin == 2)) { # A has v2; B has v1 and v3
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 2, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.row <- A.row + 1
    B.More[B.row, ] <- newTris[2, ]
    B.More[B.row + 1, ] <- newTris[3, ]
    B.row <- B.row + 2
  }
  for (i in which(fBin == 3)) { # A has v2 and v3; B has v1
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 3, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.More[A.row + 1, ] <- newTris[2, ]
    A.row <- A.row + 2
    B.More[B.row, ] <- newTris[3, ]
    B.row <- B.row + 1
  }
  for (i in which(fBin == 4)) { # A has v1; B has v2 and v3
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 4, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.row <- A.row + 1
    B.More[B.row, ] <- newTris[2, ]
    B.More[B.row + 1, ] <- newTris[3, ]
    B.row <- B.row + 2
  }
  for (i in which(fBin == 5)) { # A has v1 and v3; B has v2
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 5, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.More[A.row + 1, ] <- newTris[2, ]
    A.row <- A.row + 2
    B.More[B.row, ] <- newTris[3, ]
    B.row <- B.row + 1
  }
  for (i in which(fBin == 6)) { # A has v1 and v2; B has v3
    curFace <- faces[i, ]
    newTris <- triCondition(curFace, vert.Tab, 6, nrow(verts))
    A.More[A.row, ] <- newTris[1, ]
    A.More[A.row + 1, ] <- newTris[2, ]
    A.row <- A.row + 2
    B.More[B.row, ] <- newTris[3, ]
    B.row <- B.row + 1
  }

  A.faces <- rbind(A.faces, A.More)
  B.faces <- rbind(B.faces, B.More)

  # --- reindex vertices for A and B and pack mesh3d objects --------------------
  allVertsIndex <- cbind(allVerts, seq_len(nrow(allVerts)))
  A.vb      <- allVertsIndex[sort(unique(as.vector(A.faces))), , drop = FALSE]
  A.normals <- allNorms[, sort(unique(as.vector(A.faces))), drop = FALSE]
  B.vb      <- allVertsIndex[sort(unique(as.vector(B.faces))), , drop = FALSE]
  B.normals <- allNorms[, sort(unique(as.vector(B.faces))), drop = FALSE]

  A.it <- apply(A.faces, c(1, 2), function(x) which(A.vb[, 4] == x))
  B.it <- apply(B.faces, c(1, 2), function(x) which(B.vb[, 4] == x))

  A.vb[, 4] <- 1
  B.vb[, 4] <- 1

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
