complete <- function(directory, id=1:332){
  
  ## initialize vectors for ids and nobs
  ids <- vector("numeric")
  nobs <- vector("numeric")
  
  for (n in id){
    
    ## put number for csv file in correct format
    if(n<10){
      f <-paste('00',as.character(n),'.csv',sep='')
    }
    if(n<100 && n>9){
      f <-paste('0',as.character(n),'.csv', sep='')
    }
    if(n>99){
      f <- paste(as.character(n),'.csv',sep='')
    }
    
    ## read file and count number of completed cases
    t <- paste(directory,f,sep='/')
    m <- read.csv(t)
    
    tmp <- 0
    for(j in 1:nrow(m)){
      if(!is.na(m[j,'sulfate']) && !is.na(m[j,'nitrate'])){
        tmp <- tmp+1
      }
    }
    
    if(tmp != 0){
      ids <- c(ids,n)
      nobs <- c(nobs,tmp)
    }
    
  }
  
  #return a data frame with id's and nobs (number of complete cases)
  data.frame("id"=ids,"nobs"=nobs)
}