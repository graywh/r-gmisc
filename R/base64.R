BASE64 <- c(LETTERS, letters, as.character(seq(0,9)), "+", "/")

encode64 <- function(x) {
    return(sapply(x, function(x) {
        if (class(x) == "character")
            x <- as.numeric(charToRaw(x))
        l <- ceiling(length(x) / 3)
        a <- length(x) %% 3
        y <- c()
        m <- 256 ^ seq(2,0)
        d <- 64 ^ seq(3,0)
        for (i in seq(l)) {
            t <- x[seq(i * 3 - 2,i * 3)] * m
            t[is.na(t)] <- 0
            y[seq(i * 4 - 3,i * 4)] <- sum(t) %/% d %% 64
        }
        y <- BASE64[y + 1]
        if (a > 0)
            y[seq(l * 4 - 2 + a,l * 4)] <- "="
        paste(y, collapse="")
    }))
}

decode64 <- function(s, toChar=TRUE) {
    s <- strsplit(s, "")
    return(sapply(s, function(x) {
        l <- length(x) / 4
        a <- sum(x == "=")
        x <- x[x != "="]
        x <- match(x, BASE64) - 1
        y <- c()
        m <- 64 ^ seq(3,0)
        d <- 256 ^ seq(2,0)
        for (i in seq(l)) {
            t <- x[seq(i * 4 - 3,i * 4)] * m
            t[is.na(t)] <- 0
            y[seq(i * 3 - 2,i * 3)] <- sum(t) %/% d %% 256
        }
        if (a > 0)
            y <- y[seq(length(y) - a)]
        if (toChar)
            rawToChar(as.raw(y))
    }))
}
