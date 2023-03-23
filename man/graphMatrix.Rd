\name{graphMatrix}
\docType{methods}
\alias{graphMatrix}

\title{
Graph Laplacian type matrix
}
\description{
A symmetric matrix is returned.
}
\usage{
graphMatrix(X, algorithm=c("Vicus", "LEM", "HLLE"),
  K=10, alpha=0.9, ndim=2)
}

\arguments{
  \item{X}{
A numeric matrix (n times p).
}
  \item{algorithm}{
Algorithm to construct a graph matrix. Vicus matrix (Vicus), Graph Laplacian matrix (LEM), and Hessian Locally Linear Embedding matrix (HLLE) are available (Default: "Vicus").
}
  \item{K}{
The number of neighborhoods to construct a graph matrix (Default: 10).
}
  \item{alpha}{
An optional parameter for Vicus (Default: 0.9).
}
  \item{ndim}{
The number of lower dimension to embed the graph (Default: 2).
}
}

\value{
M: A symmetric matrix (n times n). This matrix can be applied to embedding function.
algorithm: algorithm parameter specified by argument
ndim: ndim parameter specified by argument
}
\author{Koki Tsuyuzaki}

\examples{
X <- matrix(runif(10*20), nrow=10, ncol=20)
str(graphMatrix(X, K=2), 2)
}

\keyword{methods}
\seealso{\code{\link{embedding}}}