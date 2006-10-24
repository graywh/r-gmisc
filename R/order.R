order <- function(x, ...) UseMethod("order")

order.default <- function(..., na.last = TRUE, decreasing = FALSE)
{
    if(!is.na(na.last))
        .Internal(order(na.last, decreasing, ...))
    else{ ## remove nas
        z <- list(...)
        if(any(diff(sapply(z, length)) != 0))
            stop("argument lengths differ")
        ans <- sapply(z, is.na)
        ok <- if(is.matrix(ans)) !apply(ans, 1, any) else !any(ans)
        if(all(!ok)) return(integer(0))
        z[[1]][!ok] <- NA
        ans <- do.call("order", c(z, decreasing=decreasing))
        keep <- seq(along=ok)[ok]
        ans[ans %in% keep]
    }
}

order.data.frame <- function(x, na.last=TRUE, decreasing=FALSE)
{
    len <- dim(x)[2]
    len2 <- length(na.last)
    len3 <- length(decreasing)
    if (len < len2)
        na.last <- na.last[len]
    if (len > len2)
        na.last[(len2 + 1) : len] <- TRUE
    if (len < len3)
        decreasing <- decreasing[len]
    if (len > len3)
        decreasing[(len3 + 1) : len] <- FALSE
    ox <- 1:dim(x)[1]
    for (i in len:1)
    {
        ox <- order(x[,i], order(ox) * (1 - 2 * decreasing[i]), na.last=na.last[i], decreasing=decreasing[i])
    }
    return(ox)
}
