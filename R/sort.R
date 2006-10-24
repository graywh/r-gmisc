sort <- function(x, ...) UseMethod("sort")

sort.default <- function(x, partial = NULL, na.last = NA, decreasing = FALSE,
                 method = c("shell", "quick"), index.return = FALSE, ...)
{
    if(isfact <- is.factor(x)) {
        if(index.return) stop("'index.return' only for non-factors")
	lev <- levels(x)
	nlev <- nlevels(x)
 	isord <- is.ordered(x)
        x <- c(x)
    } else
    if(!is.atomic(x))
        stop("'x' must be atomic")
    if(has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <-  x[!ina]
    }
    if(index.return && !is.na(na.last))
        stop("'index.return' only for 'na.last = NA'")
    if(!is.null(partial)) {
        if(index.return || decreasing || isfact || !missing(method))
	    stop("unsupported options for partial sorting")
        if(!all(is.finite(partial))) stop("non-finite 'partial'")
	y <- .Internal(psort(x, partial))
    }
    else {
        nms <- names(x)
        method <- if(is.numeric(x)) match.arg(method) else "shell"
        switch(method,
               "quick" = {
                   if(!is.null(nms)) {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, TRUE))
                       if(decreasing) y$x <- -y$x
                       names(y$x) <- nms[y$ix]
                       if (!index.return) y <- y$x
                   } else {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, index.return))
                       if(decreasing)
                           if(index.return) y$x <- -y$x else y <- -y
                   }
               },
               "shell" = {
                   if(index.return || !is.null(nms)) {
                       o <- sort.list(x, decreasing = decreasing)
                       y <- if (index.return) list(x = x[o], ix = o) else x[o]
                       ## names(y) <- nms[o] # pointless!
                   }
                   else
                       y <- .Internal(sort(x, decreasing))
               })
    }
    if(!is.na(na.last) && has.na)
	y <- if(!na.last) c(nas, y) else c(y, nas)
    if(isfact)
        y <- (if (isord) ordered else factor)(y, levels=seq(len=nlev),
                                              labels=lev)
    y
}

sort.data.frame <- function(x, na.last=TRUE, decreasing=FALSE)
{
    len <- dim(x)[2]
    len2 <- length(na.last)
    len3 <- length(decreasing)
    if (len < len3)
        decreasing <- decreasing[len]
    if (len > len3)
        decreasing[(len3 + 1) : len] <- FALSE
    if (len < len2)
        na.last <- na.last[len]
    if (len > len2)
        na.last[(len2 + 1) : len] <- TRUE
    for (i in len:1)
    {
        x <- x[order(x[,i], na.last=na.last[i], decreasing=decreasing[i]),]
    }
    return(x)
}
