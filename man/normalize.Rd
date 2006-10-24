\name{normalize}
\alias{normalize}
\title{Normalize/Standardize Data}
\description{
Normalizes data.
}
\synopsis{
normalize(x, margin = c(0,1,2), rename = TRUE)
}
\usage{
normalize(x, margin = 0, rename = TRUE)
normalize(x, margin, rename = TRUE)
}
\arguments{
    \item{x}{Vector, matrix, or data.frame of numeric data.}
    \item{margin}{The subscripts the normalization will be apply over.  '0' indicates all data, '1' indicates rows, and '2' indicates columns.}
    \item{rename}{Whether or not to add '\_std' to the margin names in the return value.  Ignored if margin is '0'.}
}
\details{
Uses the basic (x - mean(x))/sd(x) formula to normalize a set of data so its mean is 0 and standard deviation is 1.
}
\value{
Normalized copy of the data as a vector or matrix depending on whether or not a margin was used.
}
\author{
Will Gray\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{w.gray@vanderbilt.edu}
}
\examples{
\dontshow{
d <- data.frame(matrix(rnorm(100), ncol=5))
names(d) <- c("tvalue", "cvalue", "dvalue", "asam", "wga")
}
d.std <- normalize(d, 2)
}
\keyword{utilities}
