\name{gcd}
\alias{gcd}
\alias{lcm}
\title{GCD and LCM}
\description{
  Useful math functions that R doesn't have.
}
\usage{
gcd(a, b)

lcm(a, b)
}
\arguments{
  \item{a,b}{numbers to compute the greatest common divisor or least common multiple of.}
}
\value{
  The greatest common divisor or least common multiple.
}
\examples{
gcd(56, 42)
gcd(57, 42)
gcd(59, 42)

lcm(56, 42)
lcm(12, 42)
}
\keyword{utilities}
