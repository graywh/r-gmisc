\name{iterator}
\alias{iterator}
\title{Iterator}
\description{
Creates an iterator to iterate over a sequence of numbers without creating the entire sequence in memory.
}
\usage{
iterator(i, n=NULL, step=1)
}
\arguments{
    \item{i}{start of the sequence.  Alternatively, if \code{n} is missing, the end of the sequence and the start is assumed to be \code{1}.}
    \item{n}{optional end of the sequence when the start is specified.}
    \item{step}{increment of the sequence.}
}
\details{
Uses a list to store the state of the iterator including the current location, end, and step increment.
}
\value{
A list with class \code{"iterator"} containing the following components:
    \item{i}{the current location of the iterator.}
    \item{n}{the end of the sequence.}
    \item{s}{the incrementer.}
}
\author{
Will Gray\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{w.gray@vanderbilt.edu}
}
\keyword{utilities}
