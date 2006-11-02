iterate <- function(iter)
{
    if (iter$i == iter$n)
        return(FALSE)
    eval(eval(substitute(expression(iter$i <<- iter$i + iter$s))))
    return(TRUE)
}
