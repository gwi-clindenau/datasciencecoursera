best <- function(state, outcome) {
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
  minState = data[data$State == state, ]
  ## Return hospital name in that state with lowest 30-day death
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
