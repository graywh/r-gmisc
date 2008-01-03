golden.ratio <- (1 + sqrt(5)) / 2

fibonacci <- function(n) as.integer((golden.ratio ^ n - (1 - golden.ratio) ^ n) / sqrt(5))

fibonacci.order <- function(x)
{
    n <- as.integer(floor(log(sqrt(5) * x, golden.ratio) + 0.5))
    ifelse(fibonacci(n) == as.integer(x),
        n,
        NA)
}

fibonacci.under <- function(m) fibonacci(floor(log(sqrt(5) * m, golden.ratio) + 0.5))

is.fibonacci <- function(x) fibonacci.under(x) == x
