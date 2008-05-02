gcd <- function(a, b) {
    .internal <- function(x, y) {
        if (y == 0) {
            x
        } else {
            .internal(y, x %% y)
        }
    }
    if (length(a) > 1 || length(b) > 1) { stop("does not work element-wise") }
    a <- as.integer(abs(a))
    b <- as.integer(abs(b))
    .internal(a, b)
}

lcm <- function(a, b) {
    a * b / gcd(a, b)
}
