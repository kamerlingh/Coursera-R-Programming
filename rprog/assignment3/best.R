best <- function(state,outcome){
  ##read file into dataframe and coerce data into characters
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ##check that 'state' is valid
  valid_states <- outcomes[,7]
  if(!(state %in% valid_states)){
    stop("invalid state")
  }
  
  ##check that 'outcome' is valid and get column number for 'outcome'
  colnums <- list("heart attack"=11, "heart failure" = 17, "pneumonia"=23)
  
  if(! with(colnums, exists(outcome))){
    stop("invalid outcome")
  } else {
    colnum <- as.numeric(colnums[outcome])
  }
  
  ##make a data frame with hospital names and outcomes in alphabetical order
  state_outcomes <- split(outcomes,outcomes$State)[[state]]
  colnames(state_outcomes)[colnum]<-outcome
  state_outcomes[[outcome]] <- suppressWarnings(as.numeric(state_outcomes[[outcome]]))
  data <- state_outcomes[,c("Hospital.Name",outcome)]
  data <- data[order(data$"Hospital.Name"),]
  
  ##remove rows with NAs
  data <- data[complete.cases(data),]
  
  ##
  target <- min(data[[outcome]], na.rm=TRUE)
  
  for(i in 1:length(data[[outcome]])){
    if(data[i,2]==target){
      return(data[i,1])
    }
  }
  
}