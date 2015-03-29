rankall <- function(outcome,num="best"){
  ##read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ##check that state and outcome are valid
  ##this should be broken out into its own function
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
  
  ##for each state, find the hospital of the given rank (don't use rankhospital)
  state_outcomes <- split(outcomes,outcomes$State)
  
  df <- data.frame(0,nrows=54,ncol=2)
  
  print(length(state_outcomes))
  
  ##return a data frame with the hospital names and states
}