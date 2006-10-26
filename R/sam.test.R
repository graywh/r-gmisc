sam.test <- function(x, y=NULL, censor=NULL, ...)
{
    return(as.vector(samr(list(x = t(x), y=y, censoring.status=censor), "Quantitative", nperms=1, ...)$tt))
}
