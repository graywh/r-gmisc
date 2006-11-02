wga.test <- function(x, y, ...)
{
    n1 <- length(x)
    n2 <- length(y)
    if (n1 < 2 || n2 < 2)
        stop("Not enough samples.")
    t1 <- (n1 * (n1 - 1)) / 2
    t2 <- (n2 * (n2 - 1)) / 2
    dw1 <- sum(dist(x))
    dw2 <- sum(dist(y))
    dB <- abs(mean(x) - mean(y))
    denom <- (dw1 + dw2) / (t1 + t2)
    if (dB == 0)
    {
        wga <- 0
    }
    else if (denom == 0)
    {
        wga <- 1000
    }
    else
    {
        wga <- dB / denom
    }
    return(wga)
}
