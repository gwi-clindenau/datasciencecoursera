corr <- function(directory = "specdata", threshold = 0) {
  j <- 1
  corVec <- numeric()
  for (i in 1:332) {
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
    if (actNob > threshold) {
      corVec[j] <-cor(actualCompl$sulfate, actualCompl$nitrate)
      j <- j+1
    } 
  }  
return(corVec)
}

