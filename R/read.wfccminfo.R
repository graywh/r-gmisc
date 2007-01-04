read.wfccminfo <- function(file, sep="\t")
{
    return(read.table(file, header = TRUE, sep, na.strings = ".", row.names = 1, comment.char = ""))
}
