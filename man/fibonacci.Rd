\name{fibonacci}
\alias{fibonacci}
\alias{lucas}
\alias{is.fibonacci}
\alias{fibonacci.order}
\alias{fibonacci.under}
\alias{golden.ratio}
\title{Fibonacci and Lucas Numbers}
\description{
  Uses the Golden Ratio to find and test Fibonacci and Lucas numbers.
}
\usage{
fibonacci(n)
lucas(n)

is.fibonacci(x)

fibonacci.order(x)

fibonacci.under(m)
}
\arguments{
  \item{n}{the index of the sequence numbers to compute.}
  \item{x}{number to test for presence in the Fibonacci sequence or get the position of.}
  \item{m}{maximum limit to find the largest Fibonacci number than.}
}
\value{
  Number vector of sequence numbers.

  Boolen vector.

  Number vector of Fibonacci numbers.
}
\examples{
fibonacci(1:10)
lucas(1:10)

is.fibonacci(1:10)

fibonacci.under(100)

fibonacci.order(55)
fibonacci(1:fibonacci.order(55))
}
\keyword{utilities}
