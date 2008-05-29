\name{uniq}
\alias{uniq}
\title{Remove Repeated Elements}
\description{
  Returns a vector with consecutive repeated elements reduced.
}
\usage{
uniq(x)
}
\arguments{
  \item{x}{a vector.}
}
\details{
  Uses \code{rle} to determine which values are repeats of the previous value then removes them.
}
\value{
  A duplicate vector with consecutive repeated elements reduced to a single occurance.
}
\seealso{
  \code{\link{unique}}
  \code{\link{rle}}
}
\examples{
(x <- rep(rep(1:3,2), rep(1:2,3)))
rle(x)
uniq(x)
}
\keyword{utilities}
