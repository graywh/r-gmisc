\name{sign2}
\alias{sign2}
\title{Sign Dataset Variables}
\description{
Applies the sign of one column of a data.frame to other columns in the same data.frame.
}
\usage{
sign2(data, lead, rest)
}
\arguments{
    \item{data}{The data.frame to apply signs on.}
    \item{lead}{The name or index of the column to get the signs from.}
    \item{rest}{The names or indices of the columns to apply the sign of the lead column to.}
}
\details{
Gets the sign of a single column/variable of a data.frame and multiplies that by other columns/variables in the same data.frame.}
\value{
The modified data.frame with the signs of values in certain columns modified.
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
d <- sign2(d, lead = "tvalue", rest = c("cvalue", "dvalue", "asam", "wga"))
}
\keyword{utilities}
