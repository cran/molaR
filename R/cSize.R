#' Centroid Size Function
#'
#' Get the centroid size
#' @param x point cloud
#' @noRd

cSize <- function(x) {
	X <- scale(x, scale = FALSE)
    y <- sqrt(sum(as.vector(X)^2))
    return(y)
}
