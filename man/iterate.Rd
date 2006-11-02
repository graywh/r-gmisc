\name{iterate}
\alias{iterate}
\title{Iterate}
\description{
Iterates over an iterator.
}
\usage{
iterate(iter)
}
\arguments{
    \item{iter}{iterator.}
}
\details{
Increments the current location.
}
\value{
Whether or not the current step is valid.
}
\author{
Will Gray\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{w.gray@vanderbilt.edu}
}
\examples{
iter <- iterator(1,10)
while (iterate(iter)) print(iter$i)
}
\keyword{utilities}
