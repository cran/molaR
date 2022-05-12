# Internal look-up function for handling faces that cross the plane when cutting mesh

triCondition <- function(tri, index, con, vCount){
  v1 <- tri[1]; v2 <- tri[2]; v3 <- tri[3]
  out <- matrix(nrow = 3, ncol = 3)
  if(con == 1){
    b <- index[which(index[,1] == paste0(v2, v3, collapse = "")), 2] + vCount
    c <- index[which(index[,1] == paste0(v3, v1, collapse = "")), 2] + vCount
    out[1,] <- c(c, b, v3)
    out[2,] <- c(v1, b, c)
    out[3,] <- c(v1, v2, b)
  }
  if(con == 2){
    a <- index[which(index[,1] == paste0(v1, v2, collapse = "")), 2] + vCount
    b <- index[which(index[,1] == paste0(v2, v3, collapse = "")), 2] + vCount
    out[1,] <- c(a, v2, b)
    out[2,] <- c(v1, a, b)
    out[3,] <- c(v1, b, v3)
  }
  if(con == 3){
    a <- index[which(index[,1] == paste0(v1, v2, collapse = "")), 2] + vCount
    c <- index[which(index[,1] == paste0(v3, v1, collapse = "")), 2] + vCount
    out[1,] <- c(a, v2, v3)
    out[2,] <- c(a, v3, c)
    out[3,] <- c(v1, a, c)
  }
  if(con == 4){
    a <- index[which(index[,1] == paste0(v1, v2, collapse = "")), 2] + vCount
    c <- index[which(index[,1] == paste0(v3, v1, collapse = "")), 2] + vCount
    out[1,] <- c(v1, a, c)
    out[2,] <- c(a, v2, v3)
    out[3,] <- c(a, v3, c)
  }
  if(con == 5){
    a <- index[which(index[,1] == paste0(v1, v2, collapse = "")), 2] + vCount
    b <- index[which(index[,1] == paste0(v2, v3, collapse = "")), 2] + vCount
    out[1,] <- c(v1, a, b)
    out[2,] <- c(v1, b, v3)
    out[3,] <- c(a, v2, b)
  }
  if(con == 6){
    b <- index[which(index[,1] == paste0(v2, v3, collapse = "")), 2] + vCount
    c <- index[which(index[,1] == paste0(v3, v1, collapse = "")), 2] + vCount
    out[1,] <- c(v1, v2, b)
    out[2,] <- c(v1, b, c)
    out[3,] <- c(b, v3, c)
  }
  return(out)
}
