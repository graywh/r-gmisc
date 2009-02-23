dayofweek <- function(dates) {
    year <- as.numeric(format(dates, "%Y"))
    month <- as.numeric(format(dates, "%m"))
    day <- as.numeric(format(dates, "%d"))
    t <- c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    year <- year - (month < 3)
    return((year + year%/%4 - year%/%100 + year%/%400 + t[month] + day) %% 7 + 1)
}
