\name{roundrobin}
\alias{roundrobin}
\title{Round-Robin}
\description{
Generates round-robin schedules for a certain number of teams and rounds.
}
\usage{
roundrobin(teams, rounds = teams - 1)
}
\arguments{
    \item{teams}{single integer.  The number of teams to generate a schedule for.}
    \item{rounds}{single integer.  The number of rounds to generate a schedule for.  Defaults to \code{teams - 1} for a true round-robin.}
}
\value{
Integer matrix with each column corresponding to a team and each row corresponding to the other teams that teams play for the round.  \code{NA} represents a BYE.
}
\details{
Current based on the form Yahoo! uses in its fantasy sports applications.  I hope to add other simple forms.
}
\author{Will Gray \email{graywh@gmail.com}}
\keyword{programming}
\keyword{utilities}
