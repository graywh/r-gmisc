permutation <- function(x, n)
{
    base <- c(1, cumprod(1:n))
    idx <- rev(diff((x - 1) %% base) %/% base[1:n] + 1)
    m <- outer(idx, idx, '<=')
    m[!upper.tri(m)] <- FALSE
    colSums(m) + idx
}
