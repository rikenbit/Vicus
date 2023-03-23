.inverse_rank <- function(x){
    rank(- x, ties.method = "first")
}

.knn_adjacency <- function(X, K){
    n <- nrow(X)
    nnidx <- nn2(X, k=K+1)$nn.idx
    ii <- unlist(lapply(seq_len(n), function(x){
        rep(x, length=K)
    }))
    jj <- as.vector(t(nnidx[, -1]))
    xx <- rep(1, length=n*K)
    spMatrix(i=ii, j=jj, x=xx, nrow=n, ncol=n)
}

.spDiagMatrix <- function(x, n){
    ii <- seq_len(n)
    jj <- ii
    xx <- rep(x, length=n)
    spMatrix(i=ii, j=jj, x=xx, nrow=n, ncol=n)
}
