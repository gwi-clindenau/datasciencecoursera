pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
  sumProd <- 0
  totalSum <- 0
  meansdata = matrix(data=0, nrow=length(id), ncol=2)
  myDir <- paste(getwd(), "/", directory, "/", sep = "")
  for(i in id) {
    if (i<10) {
      filename <- paste(myDir, "00", i, ".csv", sep="")
    } else if (i<100) {
      filename <- paste(myDir, "0", i, ".csv", sep="")
    } else {
      filename <- paste(myDir, i, ".csv", sep="")
    }
    actualcsv <- read.csv(filename)
    sumTop <- mean(actualcsv[,pollutant], na.rm = TRUE) * sum(!is.na(actualcsv[,pollutant]))
    if (!is.nan(sumTop)) {
      sumProd <- sumProd + sumTop
      totalSum <- totalSum + sum(!is.na(actualcsv[,pollutant]))
    }
  }
  pollMean <- sumProd/totalSum
  print(pollMean)
}
