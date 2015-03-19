corr <- function(directory, threshold=0){
  ## initialize correlations vector
  correlations <- vector(mode="numeric")
  
  ## go through each file in the directory
  ## test whether file is above threshold
  for(i in 1:332){
    df <- complete(directory,i)
    
    ## handle numeric(0) in dataframe
    if(identical(df$nobs, numeric(0))){
      test = 0
    }else{
      test = df$nobs
    }
    
    if(test > threshold){
      t <- id2file(i,directory)
      m <- read.csv(t)
      result <- cor(m[,'nitrate'],m[,'sulfate'],use="complete.obs")
      correlations <- c(correlations,result)
    }
  }
  ## Return a numeric vector of correlations
  correlations
}