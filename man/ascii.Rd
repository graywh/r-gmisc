\name{ascii}
\alias{ASCII}
\alias{ascii}
\alias{intToChar}
\alias{charToInt}
\title{ASCII functions}
\description{
  Some useful ASCII operations that are missing in R.
}
\usage{
intToChar(i)
charToInt(ch)
}
\arguments{
  \item{i}{integer vector [0-255].  Get the ASCII equivalent for the integer code.}
  \item{ch}{vector of single characters.  Convert the characters to their ASCII codes.}
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
intToChar(72)
charToInt("G")
}
\keyword{programming}
\keyword{utilities}
