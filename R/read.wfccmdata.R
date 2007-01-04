read.wfccmdata <- function(file, sep = "\t")
{
    data <- read.table(file, header = TRUE, sep, na.strings = ".", row.names = 1)
    data <- data[,colnames(data) != "id"]
    return(data.frame(t(data)))
}
