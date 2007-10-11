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
    v <- 1:t1

    slice <- function(x, n)
    {
        x[(1:length(x) + n - 2) %% length(x) + 1]
    }

    #mat <- matrix(rep(v, t1) - rep(v, each=t1) + 1, ncol=t1) %% t1 + 1
    mat <- sapply(slice(t1:1, t2), slice, x=v)
    idx <- mapply(match, v, as.data.frame(mat))

    mat <- replace(mat, idx + 0:t2 * t1, t0)

    if (!bye)
    {
        mat <- cbind(mat, order(idx))
    }

    mat[(1:rounds - 1) %% t1 + 1,]
}
