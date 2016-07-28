#' Function will compute the DNE per face.
#'
#' This will generate each Dirichlet's normal energy for each triangular face on the surface.
#' @param plyFile a stanford PLY file 
#' compute_energy_per_face()
#'


compute_energy_per_face <- function(plyFile) {
## This puts the vertices of each face into an array with x-y-z cooridnates. 
Verts <- t(plyFile$vb[-4,])
Face <- plyFile$it
Vs <- t(Verts[Face,])
dim(Vs) <- c(3, 3, length(Face[1,]))


Vs1 <- Vs[,2,]-Vs[,1,]
Vs2 <- Vs[,3,]-Vs[,1,]

A <- rowSums(t(Vs1*Vs1))
B <- rowSums(t(Vs1*Vs2))
C <- rowSums(t(Vs2*Vs1))
D <- rowSums(t(Vs2*Vs2))

cher <- rbind(A, B, C, D)
dim(cher) <- c(2,2,length(Face[1,]))


Norms <- t(plyFile$normals[-4,])
Ns  <- t(Norms[Face,])
dim(Ns) <- c(3,3,length(Face[1,]))

Ns1 <- Ns[,2,]-Ns[,1,]
Ns2 <- Ns[,3,]-Ns[,1,]

X <- rowSums(t(Ns1*Ns1))
Y <- rowSums(t(Ns1*Ns2))
Z <- rowSums(t(Ns2*Ns1))
Q <- rowSums(t(Ns2*Ns2))

mad <- rbind(X, Y, Z, Q)
dim(mad) <- c(2, 2, length(Face[1,]))

solved <- apply(cher, 3, solve)
dim(solved) <- c(2,2,length(Face[1,]))


array1 <- array(sapply(1:length(Face[1,]), function(x) solved[,,x]%*%mad[,,x]), dim=c(2,2,length(Face[1,])))

e <- apply(array1, 3, tr)

e[which(apply(cher, 3, function(x) 1/rcond(x, '1'))>10^5)] <- 0

face_area <- apply(cher, 3, function(x) 0.5*sqrt(x[1,1]*x[2,2]-x[1,2]*x[2,1]))


Values_Per_Face <- data.frame(Dirichlet_Energy_Densities=e, Face_Areas=face_area)

return(Values_Per_Face)
}