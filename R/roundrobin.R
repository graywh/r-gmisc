roundrobin <- function(teams, rounds=teams-1)
{
    t0 <- NA
    bye <- teams %% 2 == 1
    if (bye)
    {
        teams <- teams + 1
    }
    else
    {
        t0 <- teams
    }
    t1 <- teams - 1
    t2 <- teams - 2

    slice <- function(x, n)
    {
        c(x[n:length(x)], if (n > 1) x[1:(n-1)] else c())
    }

    mat <- sapply(slice(t1:1,t2), slice, x=1:t1)
    idx <- mapply(match, 1:t1, as.data.frame(mat))

    mat <- replace(mat, idx + 0:t2 * t1, t0)

    if (!bye)
    {
        mat <- cbind(mat, order(idx))
    }

    mat[(1:rounds - 1) %% t1 + 1,]
}
