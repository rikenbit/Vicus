\name{embedding}
\docType{methods}
\alias{embedding}

\title{
Graph Embedding
}
\description{
Lower dimensions are estimated from the object returned from graphMatrix function.
}
\usage{
embedding(obj)
}

\arguments{
  \item{obj}{
Object returned from graphMatrix function.
}
}

\value{
A matrix (n times k)
}
\author{Koki Tsuyuzaki}

\examples{
X <- matrix(runif(10*20), nrow=10, ncol=20)
head(embedding(graphMatrix(X, K=2)))
}

\keyword{methods}
\seealso{\code{\link{graphMatrix}}}