# Laplacian Eigenmaps
.LEM <- function(X, K, alpha, ndim){
    # kNN-adjacency matrix
    A <- .knn_adjacency(X, K)
    # Graph Laplacian matrix
    D <- .spDiagMatrix(K, nrow(X))
    D - A
}
