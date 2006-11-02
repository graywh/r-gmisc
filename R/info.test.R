info.test <- function(x, y, ...)
{
    entropy <- function(x)
    {
        if (x == 0 || x == 1)
            return(0)
        else
            return(-((1 - x) * log(1 - x, 2) + x * log(x, 2)))
    }

    nx <- length(x)
    ny <- length(y)
    if (nx < 1)
        stop("not enough 'x' observations")
    if (ny < 1)
        stop("not enough 'y' observations")
    d <- c(x, y)
    grp <- c(rep(1, nx), rep(2, ny))
    num <- length(d)
    # A is for UPUP, B is for UPDOWN
    A <- grp[order(d, grp)]
    B <- grp[order(d,-grp)]
    xleftA <- 0
    xleftB <- 0
    scr <- 1
    for (i in 1:(num - 1))
    {
        numi <- num - i
        if (A[i] == 1)
            xleftA <- xleftA + 1
        if (B[i] == 1)
            xleftB <- xleftB + 1
        a <- i / num
        tmp <- a * entropy(xleftA / i) + (1 - a) * entropy((nx - xleftA) / numi)
        if (tmp < scr)
            scr <- tmp
        tmp <- a * entropy(xleftB / i) + (1 - a) * entropy((nx - xleftB) / numi)
        if (tmp < scr)
            scr <- tmp
    }
    return(scr)
}
