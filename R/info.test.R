info.test <- function(x, ...) UseMethod("info.test")

info.test.default <- function(x, y, ...)
{
    entropy <- function(x)
    {
        if (x == 0 || x == 1)
            return(0)
        else
            return(-((1 - x) * log(1 - x, 2) + x * log(x, 2)))
    }

    dname <- paste(deparse(substitute(x)),"and",deparse(substitute(y)))
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
    rval <- list(statistic = scr)
    return(rval)
}

info.test.formula <- function(formula, data, subset, ...)
{
    if (missing(formula)
        || (length(formula) != 3)
        || (length(attr(terms(formula[-2]), "term.labels")) != 1)
        || (length(attr(terms(formula[-3]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("info.test", c(DATA, list(...)))
    y$data.name <- DNAME
    return(y)
}
