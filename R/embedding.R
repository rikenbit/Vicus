embedding <- function(obj){
    # Eigen Decomposition
    if(obj$algorithm == "HLLE"){
        out <- eigs_sym(obj$M, k=obj$ndim+1, sigma=-1e-5)
        out <- out$vectors[, order(out$values)[-1]]
    }else{
        out <- eigen(obj$M)
        n <- nrow(obj$M)
        out <- Re(out$vectors[, seq(n-1, n-obj$ndim, by=-1)])
    }
    out
}
