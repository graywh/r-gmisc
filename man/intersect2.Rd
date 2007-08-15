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
    \item{...}{arrays of items to find the intersection of.}
}
\details{
Finds the intersection of several arrays.
}
\value{
Array of the intersection.
}
\author{
Will Gray\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{w.gray@vanderbilt.edu}
}
\examples{
\dontshow{
x2 <- 2 * (1:50)
x3 <- 3 * (1:33)
x5 <- 5 * (1:20)
}
intersect2(x2, x3, x5)
}
\keyword{utilities}
