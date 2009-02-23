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
        sum(y %% b^(1:log(y, base=b)) == 0)
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
