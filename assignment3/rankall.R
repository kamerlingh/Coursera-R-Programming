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
  
  ##get rank into numeric format if "best"
  if(num=="best"){
    num <- 1
  }
  
  
  hospital <- rep("",54)
  
  ##for each state, find the hospital of the given rank (don't use rankhospital)
  state_outcomes <- split(outcomes,outcomes$State)
  for(i in 1:54){
    
    state = names(state_outcomes[i])
    outcomes <- state_outcomes[[state]]
    colnames(outcomes)[colnum]<-outcome
    outcomes[[outcome]] <- suppressWarnings(as.numeric(outcomes[[outcome]]))
    
    data <- outcomes[,c("Hospital.Name",outcome)]
    data <- data[complete.cases(data),]
    
    num2 <- num
    
    if(num=="worst"){
      num2 <- length(data[,1])
    }
    
    data <- data[order(data$"Hospital.Name"),]
    data <- data[order(data[[outcome]]),]
    
    hospital[i] <- data[num2,1]
    
  }
  
  ##return a data frame with the hospital names and states
  state <- names(state_outcomes)
  data.frame(hospital,state)
  
}