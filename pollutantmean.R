pollutantmean <- function(directory, pollutant, id = 1:332) {
 
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)
  
  all <- vector(mode="numeric")
  
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
    
    ## get data for given pollutant
    t <- paste(directory,f,sep='/')
    m <- read.csv(t)
    d <- m[,pollutant]
    
    ## add data to vector all
    for(i in 1:length(d)){
      all<-c(all,d[i])
    }
  }
  mean(all,na.rm=TRUE)
}