gcd <- function(a, b) {
    .internal <- function(x, y) ifelse(y == 0, x, Recall(y, x %% y))
    ai <- as.integer(abs(a))
    bi <- as.integer(abs(b))
    if (any(a != ai) || any(b != bi)) warning("Coercing all parameters to integers.")
    a <- ai
    b <- bi
    l <- max(length(a), length(b))
    a <- rep(a, length.out=l)
    b <- rep(b, length.out=l)
    .internal(a, b)
}

lcm <- function(a, b) { a * b / gcd(a, b) }
