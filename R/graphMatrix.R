graphMatrix <- function(X, algorithm=c("Vicus", "LEM", "HLLE"),
    K=10, alpha=0.9, ndim=2){
    # Argument Check
    algorithm <- match.arg(algorithm)
    .checkgraphMatrix(X, K, alpha, ndim)
    M <- .flist[[algorithm]](X, K, alpha, ndim)
    list(M=M, algorithm=algorithm, ndim=ndim)
}

.checkgraphMatrix <- function(X, K, alpha, ndim){
    stopifnot(is.matrix(X))
    stopifnot(is.numeric(K))
    stopifnot(is.numeric(alpha))
    stopifnot(is.numeric(ndim))
    stopifnot(alpha > 0)
    stopifnot(alpha < 1)
    stopifnot(K >= 1)
    stopifnot(K <= nrow(X) - 1)
    stopifnot(ndim >= 1)
}

.flist <- list(
    "Vicus" = .Vicus,
    "LEM" = .LEM,
    "HLLE" = .HLLE
)
