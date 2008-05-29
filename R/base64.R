BASE64 <- c(LETTERS, letters, as.character(0:9), "+", "/")

encode64 <- function(x) {
    sapply(x, function(x) {
        if (class(x) == "character")
            x <- as.numeric(charToRaw(x))
        l <- ceiling(length(x) / 3)
        a <- length(x) %% 3
        y <- c()
        m <- 256 ^ (2:0)
        d <- 64 ^ (3:0)
        for (i in 1:l) {
            t <- x[(i * 3 - 2):(i * 3)] * m
            t[is.na(t)] <- 0
            y[(i * 4 - 3):(i * 4)] <- sum(t) %/% d %% 64
        }
        y <- BASE64[y + 1]
        if (a > 0)
            y[(l * 4 - 2 + a):(l * 4)] <- "="
        paste(y, collapse="")
    })
}

decode64 <- function(s, toChar=TRUE) {
    s <- strsplit(s, "")
    sapply(s, function(x) {
        l <- length(x) / 4
        a <- sum(x == "=")
        x <- x[x != "="]
        x <- match(x, BASE64) - 1
        y <- c()
        m <- 64 ^ (3:0)
        d <- 256 ^ (2:0)
        for (i in 1:l) {
            t <- x[(i * 4 - 3):(i * 4)] * m
            t[is.na(t)] <- 0
            y[(i * 3 - 2):(i * 3)] <- sum(t) %/% d %% 256
        }
        if (a > 0)
            y <- y[1:(length(y) - a)]
        if (toChar)
            rawToChar(as.raw(y))
    })
}
