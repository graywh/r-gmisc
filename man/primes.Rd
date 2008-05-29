\name{primes}
\alias{prime.factorization}
\title{Prime Factorization}
\description{
  Computes the prime factorization of a composite number.
}
\usage{
prime.factorization(x)
}
\arguments{
  \item{x}{length-1 numeric.}
}
\details{
  As a side effect, tests for primality using a naive approach.
}
\value{
  A matrix of prime factors in the first column and powers in the second.
}
\examples{
(x <- 8*9*25*7*11)
prime.factorization(x)
}
\keyword{utilities}
