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
decode64(x)
}
\arguments{
  \item{x}{character vector of length 1.  For \code{decode64}, it should be a Base64-encode character string.  For \code{encode64}, it should be an ASCII string.}
}
\author{Will Gray \email{graywh@gmail.com}}
\examples{
encode64("This is a string to encode.")
decode64(encode64("This is a string to encode and decode."))
}
\keyword{programming}
\keyword{utilities}
