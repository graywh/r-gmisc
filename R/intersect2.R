intersect2 <- function(...) {
    args <- list(...)
    if (length(args) < 2)
        return(args)
    return(intersect(args[[1]], unlist(do.call(intersect2, args[seq(2,length(args))]))))
}
