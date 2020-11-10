complete <- function(directory = "specdata", id = 1:332) {
  compl <- data.frame()
  for (i in id) {
    myDir <- paste(getwd(), "/", directory, "/", sep = "")
    if (i<10) {
      filename <- paste(myDir, "00", i, ".csv", sep="")
    } else if (i<100) {
      filename <- paste(myDir, "0", i, ".csv", sep="")
    } else {
      filename <- paste(myDir, i, ".csv", sep="")
    }
    actualcsv <- read.csv(filename)
    actualCompl <- actualcsv[!is.na(actualcsv$sulfate) & !is.na(actualcsv$nitrate),]
    actNob <- nrow(actualCompl)
    compl <- rbind(compl, c(i, actNob))
  }
  names(compl) [1] <- "id"
  names(compl) [2] <- "nobs"
  print(compl)
}
