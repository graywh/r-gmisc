roundrobin <- function(teams, rounds=teams - (teams %% 2 == 0), method=c("yahoo","loop","recursive"))
{
    if (teams < 4)
        stop("must have at least 4 teams")
    if (rounds < 1)
        stop("must have at least 1 round")
    method <- match.arg(method)
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
    v1 <- 1:t1
    v2 <- 0:t2
    b <- 2 * 1:(t2 / 2)

    slice <- function(x, n)
    {
        x[(1:length(x) + n - 2) %% length(x) + 1]
    }

    if (method == "recursive")
    {
        if ((log2(teams) %% 1) > 0)
            stop("recursive requires 2^n or 2^(n-1) teams.")
        if (teams == 4)
            mat <- roundrobin(4, method="yahoo")
        else
        {
            td2 <- teams / 2
            #vd2 <- 1:td2
            smat <- roundrobin(td2, method="recursive")
            smat2 <- smat + td2
            #mat <- rbind(cbind(smat, smat2), c(vd2 + td2, vd2), cbind(smat2, smat))
            mat <- rbind(cbind(smat, smat2), slice(1:teams, td2 + 1), cbind(smat2, smat))
        }
        if (bye)
        {
            idx <- c(t1, b, b-1)
            mat <- replace(mat, idx + v2 * t1, t0)
            mat <- mat[,v1]
        }
    }
    else if (method == "loop") # algorithm from Wikipedia
    {
        mat <- roundrobin(teams, method="yahoo")
        mat <- mat[order(-mat[,teams]), c(teams, 1:t1)] %% teams + 1

        if (bye)
        {
            idx <- which(mat == teams)
            mat <- replace(mat, idx, t0)
            mat <- mat[,v1]
        }
    }
    else # yahoo
    {
        #mat <- matrix((rep(v1, t1) - rep(v1, each=t1) + 1) %% t1 + 1, ncol=t1) %% t1 + 1
        #mat <- outer(v1, v1, function(x,y) (x-y+1) %% t1 + 1)
        mat <- sapply(slice(t1:1, t2), slice, x=v1)
        idx <- c(t1, b, b-1)
        mat <- replace(mat, idx + v2 * t1, t0)

        if (!bye)
        {
            mat <- cbind(mat, order(idx))
        }
    }

    mat[(1:rounds - 1) %% t1 + 1,]
}
