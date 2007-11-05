\name{strcenter}
\alias{strrjust}
\alias{strljust}
\alias{strcenter}
\title{Pad Strings}
\description{
  \code{strcenter} adds \code{char} to the left and right of \code{x} to make its length \code{n}.

  \code{strrjust} adds \code{char} to the left.

  \code{strljust} adds \code{char} to the right.
}
\usage{
  strcenter(x, n = 1, char = " ", type = "c")
  strrjust(x, n = 1, char = " ", type = "c")
  strljust(x, n = 1, char = " ", type = "c")
}
\arguments{
  \item{x}{character vector.}
  \item{n}{numeric vector for the widths of the output.  The first number is used for the first string in \code{x}, the second for the second, etc.}
  \item{char}{character string or vector of 1-characters for padding characters.  The first character is used for the first string in \code{x}, the second for the second, etc.}
  \item{type}{character string: partial matching to one of \code{c("bytes", "chars", "width")}.  See \code{\link{nchar}}.}
}
\value{
  Character vector.
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
  strrjust("hello", 10)
  strljust("hello", 10)
  strcenter("hello", 10)
}
\keyword{programming}
\keyword{utilities}
