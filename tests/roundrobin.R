library(Gmisc)

# 2 1 5 6 3 4
# 3 6 1 5 4 2
# 4 3 2 1 6 5
# 5 4 6 2 1 3
# 6 5 4 3 2 1
roundrobin(6)

# 6 5 4 3 2 1
# 5 3 2 6 1 4
# 4 6 5 1 3 2
# 3 4 1 2 6 5
# 2 1 6 5 4 3
roundrobin(6, method="loop")

# 2 1 4 3 6 5 8 7
# 3 4 1 2 7 8 5 6
# 4 3 2 1 8 7 6 5
# 5 6 7 8 1 2 3 4
# 6 5 8 7 2 1 4 3
# 7 8 5 6 3 4 1 2
# 8 7 6 5 4 3 2 1
roundrobin(8, method="recursive")
