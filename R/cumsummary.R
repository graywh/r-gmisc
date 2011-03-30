cumsummary <- function (x, values=NULL)
{
    if (!is.null(values)) {
        v <- values
    } else if (is.numeric(x) && !is.null(names(x))) {
        v <- x
        x <- names(x)
    } else {
        v <- rep(1, length(x))
    }
    x <- as.factor(x)
    l <- lapply(levels(x), function(lvl) {
        cumsum(v * (x == lvl))
    })
    names(l) <- levels(x)
    return(l)
}
