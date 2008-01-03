library(Gmisc)

# 0 1 1 2 3 5 8 13 21 34 55
fibonacci(0:10)

# 2 3 4 NA 5 NA NA 6 NA NA
fibonacci.order(1:10)

# 1 2 3 5 5 5 8 8 8 8
fibonacci.under(1:10)

# T T T F T F F T F F
is.fibonacci(1:10)
