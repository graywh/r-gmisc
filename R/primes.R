prime.factorization <- function(x) {
    sqx <- sqrt(x)
    numbers <-  if (x < 100) {
                    seq(2,sqx)
                } else {
                    c(2, 3, seq(5, sqx, by=6), seq(7, sqx, by=6))
                }
    candid <- numbers[(x %% numbers) == 0]
    rem <- outer(candid, candid, '%%')
    fac <- apply(rem, 1, function(x) { sum(x==0) })
    prim.fac <- candid[fac == 1]

    how.many.times <- function(y, b) {
        sum(y %% b^seq(log(y, base=b)) == 0)
    }

    pow <- unlist(lapply(prim.fac, how.many.times, y=x))
    maybe <- x / prod(ppa <- prim.fac ^ pow)
    out <- cbind(prim.fac, pow)
    if (maybe > 1) { out <- rbind(out, c(maybe, 1)) }
    return(out[order(out[,1]),])
}

sieve.of.eratosthenes <- function(limit) {
    is.prime = rep(TRUE, limit)
    i <- 2
    x <- floor(sqrt(limit)) + 1
    while (i < x) {
        if (is.prime[i]) {
            is.prime[seq.int(from=min(i*i,limit),to=limit,by=i)] <- FALSE
        }
        i <- i + 1
    }
    return(which(is.prime))[-1]
}

is.prime.naive <- function(x) {
    x <- abs(x)
    k <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53)
    if (x == 1) return(FALSE)
    if (x %in% k) return(TRUE)
    if (any(x %% k == 0)) return(FALSE)
    sqx <- floor(sqrt(x))
    for (i in seq(5, sqx+1, 6)) {
        if (x %% i == 0) return(FALSE)
        if (x %% (i+2) == 0) return(FALSE)
    }
    return(TRUE)
}

miller.rabin <- function(x, k=NULL) {
    pow <- function(x, y, z) { (x ^ y) %% z }
    x <- abs(x)
    if (x == 1) return(FALSE)
    if (x == 2) return(TRUE)
    if (x %% 2 == 0) return(FALSE)
    x1 <- x - 1
    d <- x1
    s <- 0
    while (d %% 2 == 0) {
        d <- d / 2
        s <- s + 1
    }
    if (is.null(k)) {
        for (a in seq(2, min(x1, floor(2 * log(x) ^ 2))))
            if (pow(a,d,x) != 1 && all(pow(a, (2 ** seq(0, s-1)) * d, x) != x1))
                return(FALSE)
    } else {
        for (j in seq(0, k-1)) {
            a <- sample(x1, 1)
            if (pow(a,d,x) != 1 && all(pow(a, (2 ** seq(0, s-1)) * d, x) != x1))
                return(FALSE)
        }
    }
    return(TRUE)
}

nearprimes <- function(x) {
    if (is.prime.naive(x)) {
        a <- x
        b <- x
    } else {
        a <- nextprime(x)
        b <- prevprime(x)
    }
    return(c(b,a))
}

prevprime <- function(x) {
    prevn <- function(n) {
        if (n == 0) return(1)
        if (n == 1) return(2)
        return(n - 1)
    }
    y <- x - prevn(x %% 6)
    while (!is.prime.naive(y))
        y <- y - prevn(y %% 6)
    return(y)
}

nextprime <- function(x) {
    nextn <- function(n) {
        if (n == 0) return(1)
        if (n == 5) return(2)
        return(5 - n)
    }
    y <- x + nextn(x %% 6)
    while (!is.prime.naive(y))
        y <- y + nextn(y %% 6)
    return(y)
}
