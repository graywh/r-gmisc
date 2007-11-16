\name{baseConvert}
\alias{baseConvert}
\title{Number Base Conversion}
\description{
  Converts numbers from one base to another.
}
\usage{
baseConvert(x, target, base = 10)
}
\arguments{
  \item{x}{number to convert.  If \code{base} is greater than 10, \code{x} must be mode character.  Otherwise, numeric is allow.}
  \item{target}{target base to convert \code{x} to.}
  \item{base}{starting base of \code{x}.}
}
\details{
  Uses multiplication to convert the number to base 10 and integer division and modulo to convert that to the target base.  The starting and target bases must be in the range [2,36].
}
\value{
  Character representation of the number in the new base.
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
baseConvert(231, 16)
baseConvert(231, 2)
baseConvert("7A", 10, 16)
baseConvert("100110110", 10, 2)
}
\keyword{programming}
\keyword{utilities}
