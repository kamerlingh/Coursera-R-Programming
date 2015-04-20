rankhospital <- function(state,outcome,num="best"){
  ##read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ##check that state and outcome are valid
  valid_states <- outcomes[,7]
  if(!(state %in% valid_states)){
    stop("invalid state")
  }
  
  colnums <- list("heart attack"=11, "heart failure" = 17, "pneumonia"=23)
  
  if(! with(colnums, exists(outcome))){
    stop("invalid outcome")
  } else {
    colnum <- as.numeric(colnums[outcome])
  }
  
  ##make a data frame with (alphabetical) hospital names sorted by outcome rate
  state_outcomes <- split(outcomes,outcomes$State)[[state]]
  colnames(state_outcomes)[colnum]<-outcome
  state_outcomes[[outcome]] <- suppressWarnings(as.numeric(state_outcomes[[outcome]]))
  data <- state_outcomes[,c("Hospital.Name",outcome)]
  data <- data[complete.cases(data),]
  data <- data[order(data$"Hospital.Name"),]
  data <- data[order(data[[outcome]]),]
  
  if(num=="best"){
    num <- 1
  }
  if(num=="worst"){
    num <- length(data[[outcome]])
  }
  
  ##return hospital in that state with given rank for 30-day death rate
  return(data[num,1])
}