uniq <- function(x) {
    x[rle(x)$lengths == 1]
}
