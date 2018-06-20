cons <- c

car <- function(x) { x[1] }

cdr <- function(x) { x[-1] }

notf <- function(x) { function(...) { !x(...) } }
