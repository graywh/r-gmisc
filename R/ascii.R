ASCII <- sapply(0:255, function(x) parse(text=paste("\"\\", structure(x, class="octmode"), "\"", sep=""))[[1]])

intToChar <- function(i) ASCII[i %% 256 + 1]

charToInt <- function(ch) match(ch, ASCII) - 1
