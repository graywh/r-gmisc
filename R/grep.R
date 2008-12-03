grep <- function(pattern, x, ignore.case=FALSE, extended=TRUE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE) {
    if (!is.character(x))
        x <- structure(as.character(x), names=names(x))
    l <- length(x)
    v <- .Internal(grep(as.character(pattern), x, ignore.case, extended, value, perl, fixed, useBytes))
    if (value)
        return(v)
    else
        return(1:l %in% v)
}
