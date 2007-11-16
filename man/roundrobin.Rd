\name{roundrobin}
\alias{roundrobin}
\title{Round-Robin Schedule}
\description{
  Generates round-robin schedules for a certain number of teams and rounds.
}
\usage{
roundrobin(teams, rounds = teams - (teams \%\% 2 == 0),
           method = c("yahoo", "loop", "recursive"))
}
\arguments{
  \item{teams}{integer (length 1 vector).  The number of teams to generate a schedule for.}
  \item{rounds}{integer (length 1 vector).  The number of rounds to generate a schedule for.  Defaults to \code{teams - (teams \%\% 2 == 0)} for a true round-robin.}
  \item{method}{character (length 1 vector).  See details.}
}
\value{
  Integer matrix with each column corresponding to a team and each row corresponding to the other teams that teams play for the round.  \code{NA} represents a BYE.
}
\details{
  Method \code{"yahoo"} uses the same format Yahoo! uses for its fantasy sports applications.

  Method \code{"loop"} uses a simple algorithm found on Wikipedia.

  Method \code{"recursive"} is for tournaments with \code{teams = 2 ** k}.
}
\author{Will Gray \email{graywh@gmail.com}}
\keyword{programming}
\keyword{utilities}
