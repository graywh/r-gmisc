\name{intersect2}
\alias{intersect2}
\title{Intersect improved}
\description{
  Get the intersection of more than two vectors.
}
\usage{
intersect2(...)
}
\arguments{
  \item{...}{vectors of items to find the intersection of.}
}
\details{
  Recursively finds the intersection of several vectors.
}
\value{
  Array of the intersection.
}
\author{Will Gray \email{graywh@gmail.com}}
\seealso{
  \code{\link{intersect}}
}
\examples{
  \dontshow{
    x2 <- 2 * (1:50)
    x3 <- 3 * (1:33)
    x5 <- 5 * (1:20)
  }
  intersect2(x2, x3, x5)
}
\keyword{manip}
\keyword{utilities}
