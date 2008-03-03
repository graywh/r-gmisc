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
decode64(s, toChar = TRUE)
}
\arguments{
  \item{x}{Character vector or list of numeric vectors of ASCII byte codes.}
  \item{s}{Base64-encoded character strings.}
  \item{toChar}{Logical.  If \code{TRUE}, return the result as character, otherwise numeric ASCII byte codes.}
}
\value{
\code{encode64} returns base64-encoded character strings.

\code{decode64} returns a character vector or a list of numeric vectors of ASCII byte codes.
}
\examples{
encode64("This is a string to encode.")
decode64(encode64("This is a string to encode and decode."))
}
\keyword{programming}
\keyword{utilities}
