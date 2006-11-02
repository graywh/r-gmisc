iterator <- function(i, n=NULL, step=1)
{
    if (missing(n))
    {
        n <- i
        i <- 0
    }
    else
    {
        i <- i - 1
    }
    result <- list(i=i, n=n, s=step)
    class(result) <- "iterator"
    return(result)
}
