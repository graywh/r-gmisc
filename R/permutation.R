permutation <- function(x, n) {
    result <- c()
    choose <- 1:n
    x <- (x - 1) %% factorial(n)
    base <- c(rev(cumprod(1:(n-1))), 1)
    for (i in 1:n) {
        j <- x %/% base[i] + 1
        result[i] <- choose[j]
        choose <- choose[-j]
        x <- x %% base[i]
    }
    return(result)
}
