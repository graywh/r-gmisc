\name{fibonacci}
\alias{fibonacci}
\alias{is.fibonacci}
\alias{fibonacci.under}
\title{Fibonacci Numbers}
\description{
  Finds the Nth Fibonacci number in the sequence.  Tests for a Fibonacci number.
}
\usage{
fibonacci(n)

is.fibonacci(x)

fibonacci.under(m)
}
\arguments{
  \item{n}{the integer sequence numbers of the Fibonacci numbers to compute.}
  \item{x}{number to test for presence in the Fibonacci sequence.}
  \item{m}{maximum limit to find the largest Fibonacci number than.}
}
\value{
  Number vector of Fibonacci numbers.

  Boolen vector.

  Number vector of Fibonacci numbers.
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
fibonacci(1:10)

is.fibonacci(1:10)

fibonacci.under(100)

fibonacci.order(55)
fibonacci(1:fibonacci.order(55))
}
\keyword{programming}
\keyword{utilities}
