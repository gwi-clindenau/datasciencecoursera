id <- 1:332
directory <- "specdata"
pollutant <- "nitrate"
sumProd = 0
totalSum = 0
meansdata = matrix(data=0, nrow=length(id), ncol=2)
setwd(paste("/Users/chris/develop/datasciencecoursera/Rprogramming_week2", "/", directory, sep = ""))
for(i in id) {
    if (i<10) {
      filename <- paste("00", i, ".csv", sep="")
    } else if (i<100) {
      filename <- paste("0", i, ".csv", sep="")
    } else {
      filename <- paste(i, ".csv", sep="")
    }
    print(filename)
    actualcsv = read.csv(filename)
    sumTop <- mean(actualcsv[,pollutant], na.rm = TRUE) * sum(!is.na(actualcsv[,pollutant]))
    print(sumTop)
    if (!is.nan(sumTop)) {
      sumProd <- sumProd + sumTop
      totalSum <- totalSum + sum(!is.na(actualcsv[,pollutant]))
    }
    #meansdata[i,1] <- mean(actualcsv[,pollutant], na.rm = TRUE)
    #meansdata[i,2] <- sum(!is.na(actualcsv[,pollutant]))
    print(sumProd)
    print(totalSum)
}
    
  pm <- sumProd/totalSum
  print(pm)
  

##### part2

id <- 1:1
directory <- "specdata"
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
  print(filename)
  actualcsv <- read.csv(filename)
  actualCompl <- actualcsv[!is.na(actualcsv$sulfate) & !is.na(actualcsv$nitrate),]
  actNob <- nrow(actualCompl)
  compl <- rbind(compl, c(i, actNob))
  a <- is.na(actualcsv$sulfate)
  b <- is.na(actualcsv$nitrate)
}
names(compl) [1] <- "id"
names(compl) [2] <- "nobs"

### part 3

directory <- "specdata"
threshold <- 150
id <- 1:332
j <- 1
cr <- vector()
complThr <- data.frame()
for (i in id) {
  myDir <- paste(getwd(), "/", directory, "/", sep = "")
  if (i<10) {
    filename <- paste(myDir, "00", i, ".csv", sep="")
  } else if (i<100) {
    filename <- paste(myDir, "0", i, ".csv", sep="")
  } else {
    filename <- paste(myDir, i, ".csv", sep="")
  }
  print(filename)
  actualcsv <- read.csv(filename)
  actualCompl <- actualcsv[!is.na(actualcsv$sulfate) & !is.na(actualcsv$nitrate),]
  actNob <- nrow(actualCompl)
  if (actNob > threshold) {
    cr[j] <-cor(actualCompl$sulfate, actualCompl$nitrate)
    j <- j+1
  } 
}
print(cr)
