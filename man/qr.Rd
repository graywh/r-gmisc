\name{qrcode}
\alias{encodeQR}
\alias{decodeQR}
\title{QR code Functions}
\description{
  Encode and decode alphanumeric QR codes.
}
\usage{
encodeQR(x)
decodeQR(s)
}
\arguments{
  \item{x}{Alphanumeric character vector.}
  \item{s}{Alphanumeric QR binary code character vector.}
}
\value{
\code{encode64} returns QR-encoded character strings.

\code{decode64} returns a character vector.
}
\examples{
encodeQR("This is a string to encode.")
decodeQR(encodeQR("This is a string to encode and decode."))
}
\keyword{programming}
\keyword{utilities}
