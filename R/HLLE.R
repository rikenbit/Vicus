# Hessian Locally Linear Embedding
.HLLE <- function(X, K, alpha, ndim){
    # Setting
    n <- nrow(X)
    hs <- ndim * (ndim + 1)/2
    ii <- jj <- xx <- list()
    # kNN
    nnidx <- nn2(data = X, query = X, k = K+1,
        treetype = "kd", "standard", eps = 0)$nn.idx
    # Hessian
    for(i in seq_len(n)){
        Nui <- X[nnidx[i, ], , drop = FALSE]
        Nui <- sweep(Nui, 2, colMeans(Nui), "-")
        tc <- svd(Nui, nu = ndim, nv = 0)$u
        Xi <- cbind(1, tc, tc^2, apply(combn(seq_len(ndim),
            2), 2, function(x) tc[, x[1]] * tc[, x[2]]))
        tHi <- qr.Q(qr(Xi))[, -(1:(ndim + 1)), drop = FALSE]
        ii[[i]] <- rep(nnidx[i, ], hs)
        jj[[i]] <- rep((i-1)*hs + (1:hs), each = ncol(nnidx))
        xx[[i]] <- as.vector(tHi)
    }
    if(length(ii[[1]]) != length(xx[[1]])){
        stop("K is too small. Please specify the larger value.")
    }
    H <- spMatrix(
        i = unlist(ii, FALSE, FALSE),
        j = unlist(jj, FALSE, FALSE),
        x = unlist(xx, FALSE, FALSE),
        nrow = n, ncol = n * hs)
    as(tcrossprod(H), "generalMatrix")
}
