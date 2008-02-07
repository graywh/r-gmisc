\name{base64}
\alias{BASE64}
\alias{base64}
\alias{Base64}
\alias{encode64}
\alias{decode64}
\title{Base64 Functions}
\description{
  Encode and decode in Base64.
}
\usage{
encode64(x)
decode64(s)
}
\arguments{
  \item{x}{numeric vector of ASCII byte codes.  All values should be \code{> 1} and \code{< 256}}
  \item{s}{a base64-encoded character string.}
}
\author{Will Gray \email{graywh@gmail.com}}
\value{
\code{encode64} returns a single character string of the base64-encoding.

\code{decode64} returns a numeric vector of ASCII byte codes.
}
\examples{
encode64("This is a string to encode.")
decode64(encode64("This is a string to encode and decode."))
}
\keyword{programming}
\keyword{utilities}
