id2file <- function(n,directory){
  
  if(n<10){
    f <-paste('00',as.character(n),'.csv',sep='')
  }
  if(n<100 && n>9){
    f <-paste('0',as.character(n),'.csv', sep='')
  }
  if(n>99){
    f <- paste(as.character(n),'.csv',sep='')
  }
  
  paste(directory,f,sep='/')
  
}