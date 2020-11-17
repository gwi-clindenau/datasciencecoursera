rankhospital <- function(state, outcome, num) {
  ## surpress warning
  options(warn = - 1)  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- outcomeData[,c(2, 7, 11, 17 ,23)]
  data[,3:5] <- lapply(data[,3:5], as.numeric)
  ## Check that state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
    stop ("invalid outcome")
  } else if (!is.element(state, data$State)) {
    stop ("invalid State")
  }
  ## Return hospital name in that state with the given rank of 30-day death rate
  rankedData <- data[data$State == state, ]
  if (outcome == "heart attack") {
    rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, rankedData$Hospital.Name),]
  } else if (outcome == "heart failure") {
    rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, rankedData$Hospital.Name),]
  } else if (outcome == "pneumonia") {
    rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, rankedData$Hospital.Name),]
  }
  if (num == "best") {
    rankedHosp = rankedData[1,1]
  } else if (num =="worst") {
    if (outcome == "heart attack") {
      rankedData <- rankedData[complete.cases(rankedData[ , 3]), ]
    } else if (outcome == "heart failure") {
      rankedData <- rankedData[complete.cases(rankedData[ , 4]), ]
    } else if (outcome == "pneumonia") {
      rankedData <- rankedData[complete.cases(rankedData[ , 5]), ]
    }
    rankedHosp = rankedData[nrow(rankedData),1]
  } else rankedHosp = rankedData[num,1]
  print(rankedHosp)
}