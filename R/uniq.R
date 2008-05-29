uniq <- function(x) {
    x[c(TRUE, x[-1] != x[-length(x)])]
}
