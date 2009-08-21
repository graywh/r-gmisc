draftpicks <- function(teams, rounds, method=c("reverse", "straight")) {
    method <- match.arg(method)
    if (method %in% c("straight","reverse")) {
        x <- matrix(seq(teams*rounds), rounds, teams, TRUE)
    }
    if (method == "reverse") {
        y <- seq(rounds/2)*2
        x[y,] <- x[y,rev(seq(teams))]
    }
    return(x)
}

draftorder <- function(teams, rounds, method=c("reverse", "straight")) {
    method <- match.arg(method)
    if (method == "straight") {
        x <- matrix(rep(seq(teams), each=rounds), rounds)
    } else if (method == "reverse") {
        x <- matrix(rep(c(seq(teams), rev(seq(teams))), (rounds+1)/2), ncol=teams, byrow=TRUE)[seq(rounds),]
    }
    return(x)
}
