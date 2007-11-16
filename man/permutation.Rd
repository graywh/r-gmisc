\name{permutation}
\alias{permutation}
\title{Permutations}
\description{
  Gets a permutation of the integers 1-N by its index 1-N!.
}
\usage{
permutation(x, n)
}
\arguments{
  \item{x}{number (length 1 vector).  The index of the permutation to generate.  Must be less than \code{factorial(n)}}
  \item{n}{number (length 1 vector).  The number of items in the permutation.}
}
\value{
  Number vector of the integers 1-n re-ordered.
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
  for (i in 1:24) print(permutation(i, 4))
}
\keyword{programming}
\keyword{utilities}
