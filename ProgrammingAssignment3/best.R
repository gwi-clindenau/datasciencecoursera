best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  minState = data[data$State == state, ]
  if (outcome == "heart attack") {
      minHos <- with(minState, Hospital.Name[Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE)])
  } else if (outcome == "heart failure") {
      minHos <- with(minState, Hospital.Name[Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)])
  } else if (outcome == "pneumonia") {
      minHos <- with(minState, Hospital.Name[Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE)])
  }
  minHos <- minHos[!is.na(minHos)]
  minHos <- sort(minHos)
  print(minHos[1])
}

lapply(unique(data$State), best, outcome = "pneumonia") #best Tester

rankhospital <- function(state, outcome, num) {
  ## surpress warning
  ## options(warn = - 1)  
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

unique(data$State)
b <- sort(unique(data$State))

rankallData <- data.frame(Hospital=character(), State=character())
rankallData[3, 1] <- rankedData[1,4]
rankallData[3, 2] <- sort(unique(data$State)) [14]
state ="MD"
rankedData <- data[data$State == sort(unique(data$State)) [3], ]
rankedData <- rankedData[order(rankedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, rankedData$Hospital.Name),]rankedHosp = rankedData[,1]
rankedData <- rankedData[complete.cases(rankedData[ , 3]), ]
rankedData[nrow(rankedData), 1]


beatgetwd()
setwd("/Users/chris/develop/datasciencecoursera/ProgrammingAssignment3")
outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- outcomeData[,c(2, 7, 11, 17 ,23)]
data[,3:5] <- lapply(data[,3:5], as.numeric)
state <-  "BLUBB"
states <- c("A", "B", "C")
if (is.element(state, states) == FALSE) print "ja"
is.element(state, states)
tapply(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data$state, min)
sapply(split(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data$State), FUN=min)
length(data$State)
length(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
mintest <- with(data, Hospital.Name[Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
minStat =data[data$State == state, ]
minStat
state <- "TX"
minState = data[data$State == state, ]
minHosp <- with(minState, Hospital.Name[Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)])
minHosp <- minHosp[!is.na(minHosp)]
min(minState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
minHosp
