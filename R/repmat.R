#' Replicate a matrix
#'
#' This function replicates a matrix
#' @param a vector or matrix to replicate
#' @param n number of replicates in n-direction
#' @param m number of replicates in m-direction
#'
#' 
#' repmat
#' @noRd

repmat <- function(a, n, m = n) {
    if (length(a) == 0) return(c())
    if (!is.numeric(a) && !is.complex(a))
        stop("Argument 'a' must be a numeric or complex.")
    if (is.vector(a))
        a <- matrix(a, nrow = 1, ncol = length(a))
    if (!is.numeric(n) || !is.numeric(m) ||
        length(n) != 1 || length(m) != 1)
        stop("Arguments 'n' and 'm' must be single integers.")
    n <- max(floor(n), 0)
    m <- max(floor(m), 0)
    if (n <= 0 || m <= 0)
        return(matrix(0, nrow = n, ncol = m))

	matrix(1, n, m) %x% a  # Kronecker product
}