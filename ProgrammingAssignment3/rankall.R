rankall <- function(outcome, num = "best") {
  ## surpress warning
  ## options(warn = - 1)  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- outcomeData[,c(2, 7, 11, 17 ,23)]
  data[,3:5] <- lapply(data[,3:5], as.numeric)
  ## Check that outcome is valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
    stop ("invalid outcome")
  } 
  rankallData <- data.frame(Hospital=character(), State=character())
  ## For each state, find the hospital of the given rank
  for (i in 1:length(unique(data$State))) {
    rankedData <- data[data$State == sort(unique(data$State)) [i], ]
    if (outcome == "heart attack") {
      rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, rankedData$Hospital.Name),]
    } else if (outcome == "heart failure") {
      rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, rankedData$Hospital.Name),]
    } else if (outcome == "pneumonia") {
      rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, rankedData$Hospital.Name),]
    }
    if (num == "best") {
      rankallData[i, 1] <- rankedData[1,1]
      rankallData[i, 2] <- sort(unique(data$State)) [i]
    } else if (num =="worst") {
      if (outcome == "heart attack") {
        rankedData <- rankedData[complete.cases(rankedData[ , 3]), ]
      } else if (outcome == "heart failure") {
        rankedData <- rankedData[complete.cases(rankedData[ , 4]), ]
      } else if (outcome == "pneumonia") {
        rankedData <- rankedData[complete.cases(rankedData[ , 5]), ]
      }
      rankedHosp = rankedData[nrow(rankedData),1]
      rankallData[i, 1] <- rankedData[nrow(rankedData),1]
      rankallData[i, 2] <- sort(unique(data$State)) [i]
    } else {
      rankallData[i, 1] <- rankedData[num,1]
      rankallData[i, 2] <- sort(unique(data$State)) [i]
    }
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(rankallData)
}