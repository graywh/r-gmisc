baseConvert <- function(x, target, base=10, ...)
{
    # Value -> Digit
    characters <- c("0", "1", "2", "3", "4", "5",
                    "6", "7", "8", "9", "A", "B",
                    "C", "D", "E", "F", "G", "H",
                    "I", "J", "K", "L", "M", "N",
                    "O", "P", "Q", "R", "S", "T",
                    "U", "V", "W", "X", "Y", "Z")
    # Digit -> Value
    numbers <- 0:35
    names(numbers) <- characters
    if (is.numeric(x))
        x <- abs(x)
    if (base != 10 && !is.character(x))
        stop("Parameter x must be of mode character for bases other than decimal.")
    if (base < 2 || base > 36)
        stop("Base of x must be [2,36]")
    if (target < 2 || target > 36)
        stop("Target base for x must be [2,36]")
    if (base != 10)
    {
        x <- strsplit(x, "")
        if (any(!x %in% characters[1:base]))
            stop("Invalid number for base.")
    }
    result <- c()
    for (i in 1:length(x))
    {
        # Convert to base 10
        sum <- 0
        if (base != 10)
        {
            for (l in x[[i]])
            {
                sum <- sum * base + numbers[l]
            }
            names(sum) <- NULL
        }
        else
            sum <- x[i]
        # Convert to new base
        if (target != 10)
        {
            str <- c()
            while (sum > 0)
            {
                str <- c(characters[sum %% target + 1] , str)
                sum <- floor(sum / target)
            }
            result[i] <- paste(str, collapse="")
        }
        else
            result[i] <- sum
    }
    return(result)
}
