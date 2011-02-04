permutation <- function(x, n) {
    result <- c()
    choose <- seq(n)
    x <- (x - 1) %% factorial(n)
    base <- c(rev(cumprod(seq(n-1))), 1)
    for (i in seq(n)) {
        j <- x %/% base[i] + 1
        result[i] <- choose[j]
        choose <- choose[-j]
        x <- x %% base[i]
    }
    return(result)
}
