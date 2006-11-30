intersect2 <- function(...)
{
    args <- list(...)
    if (length(args) < 2) return(args)
    intersect(args[[1]], unlist(do.call(intersect2, args[2:length(args)])))
}
