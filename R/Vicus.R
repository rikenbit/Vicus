# Vicus
.Vicus <- function(X, K, alpha, ndim){
    # kNN-adjacency matrix
    A <- .knn_adjacency(X, K)
    n <- nrow(A)
    ii <- rep(0, length=n*K)
    jj <- ii
    xx <- ii
    count <- 1
    for(i in seq_len(n)){
        V_i <- c(which(A[i, ] != 0), i)
        W_i <- as.matrix(A[V_i, V_i])
        colnames(W_i) <- V_i
        rownames(W_i) <- V_i
        # Normalized Transition Matrix
        W_i <- W_i + 1e-20
        normTransMat <- W_i / rowSums(W_i)
        temp <- (1 - alpha) * solve(diag(nrow(normTransMat)) - alpha * normTransMat)
        beta_i <- temp[K+1, ]
        for(j in seq_len(n)){
            if(j %in% V_i && i != j){
                ii[count] <- i
                jj[count] <- j
                index_of_j <- which(colnames(W_i) == j)
                xx[count] <- as.numeric(beta_i[index_of_j] / (1 - beta_i[K + 1]))
                count <- count + 1
            }
        }
    }
    # Sparse Matrix
    B <- spMatrix(i=ii, j=jj, x=xx, nrow=n, ncol=n)
    # Vicus Matrix
    D <- .spDiagMatrix(1, nrow(X))
    as(crossprod(D - B), "generalMatrix")
}
