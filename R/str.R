strrjust <- function(x, n=1, char=" ", type="c") {
    if (nchar(char, type=type)[1] > 1)
        char <- strsplit(char[1], NULL)[[1]]
    f <- function(x, n, c) {
        if (nchar(x) < n) {
            paste(paste(rep(c, n-nchar(x, type=type)), collapse=""), x, sep="")
        } else {
            x
        }
    }
    mapply(f, x, n, char, USE.NAMES=FALSE)
}

strljust <- function(x, n=1, char=" ", type="c") {
    if (nchar(char, type=type)[1] > 1) {
        char <- strsplit(char[1], NULL)[[1]]
    }
    f <- function(x, n, c) {
        if (nchar(x) < n) {
            paste(x, paste(rep(c, n-nchar(x, type=type)), collapse=""), sep="")
        } else {
            x
        }
    }
    mapply(f, x, n, char, USE.NAMES=FALSE)
}

strcenter <- function(x, n=1, char=" ", type="c") {
    if (nchar(char, type=type)[1] > 1) {
        char <- strsplit(char[1], NULL)[[1]]
    }
    lx <- nchar(x, type=type)
    r <- ceiling((n - lx) / 2) + lx
    strrjust(strljust(x, r, char), n, char)
}

strrev <- function(x) {
    collapse(lapply(strsplit(x, NULL), rev))
}

collapse <- function(x) {
    sapply(x, paste, collapse="")
}
