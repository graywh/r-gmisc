QRALPHA <- c(0:9, LETTERS, c(' ','$','%','*','+','-','.','/',':'))
QRCODES <- structure(0:44, names=QRALPHA)

decodeQR <- function(x) {
    sapply(gsub('[ \n]', '', x), function(y) {
        n <- nchar(y)
        if (n %% 11 == 0) {
            z <- NULL
        } else {
            n <- n %/% 11 * 11
            z <- QRALPHA[baseConvert(substring(y, n+1, n+6), 10, 2) + 1]
        }
        y <- baseConvert(substring(y, seq(1,n,11), seq(11,n,11)), 10, 2)
        paste(c(QRALPHA[c(rbind(y %/% 45, y %% 45)) + 1], z), collapse='')
    })
}

encodeQR <- function(s) {
    sapply(strsplit(toupper(s), ''), function(y) {
        n <- length(y)
        if (n %% 2 == 1) {
            z <- strrjust(baseConvert(QRCODES[y[n]], 2), 6, '0')
            n <- n - 1
        } else {
            z <- NULL
        }
        a1 <- y[seq(1,n,2)]
        a2 <- y[seq(2,n,2)]
        paste0(c(strrjust(baseConvert(QRCODES[a1] * 45 + QRCODES[a2], 2), 11, '0'), z), collapse=' ')
    })
}
