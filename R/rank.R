rank <- function(x, ...) UseMethod("rank")

rank.default <- function(x, na.last = TRUE,
                 ties.method=c("average", "first", "random", "max", "min"), ...)
{
    nas <- is.na(x)
    ties.method <- match.arg(ties.method)
    y <- switch(ties.method,
                "average"= , "min"= , "max" =
                .Internal(rank(   x[!nas], ties.method)),
                "first" = sort.list(sort.list(x[!nas])),
                "random" = sort.list(order(   x[!nas], runif(sum(!nas)))))
    if(!is.na(na.last) && any(nas)) {
	## the internal code has ranks in [1, length(y)]
	storage.mode(x) <- "double"
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    x[!nas] <- y
	    if(!NAkeep) x[nas] <- (length(y) + 1:1):length(x)
	} else {
	    len <- sum(nas)
	    x[!nas] <- y + len
	    x[nas] <- 1 : len
	}
	y <- x
    } else names(y) <- names(x)[!nas]
    y
}

rank.data.frame <- function(data, rev=rep(FALSE, dim(data)[2]), ties.break=NULL)
{
    if (mode(rev) != mode(logical()))
        stop("Parameter rev must be of mode logical.")

    ranks <- data.frame(matrix(0, dim(data)[1], 0))
    row.names(ranks) <- row.names(data)
    cols <- colnames(data)
    rev <- c(rev, rep(FALSE, dim(data)[2] - length(rev)))

    for (i in 1:length(cols))
    {
        n <- cols[i]
        l <- length(n)
        if (substr(n, l-4,l) == "_rank")
            ranks[[n]] <- data[,n]
        else
            ranks[[paste(n, "_rank", sep="")]] <- order(order(data[,i], decreasing=rev[i]))
    }

    ranks$ranksum <- apply(ranks, 1, sum)
    if (missing(ties.break))
        ranks$rank <- rank(ranks$ranksum)
    else
        ranks$rank <- order(do.call(order, cbind(ranks$ranksum, data[,ties.break])))

    return(ranks)
}
