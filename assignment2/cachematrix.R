#This file contains two functions that can create a special object 
#that stores a matrix (assumed to be invertible) and caches the 
#inverse of the matrix.

#This function returns a list of functions that can store ("set") a 
#matrix "x", get the matrix, set the inverse of the matrix ("inverse"), 
#and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
  
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
  
    get <- function() x
  
    setinverse <- function(given_inverse) inverse <<- given_inverse
  
    getinverse <- function() inverse
  
    list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#This function first checks whether the inverse of the speical "vector"
#created above has already been cached and returns it if so. If not, 
#it computes the inverse, caches it in the special "vector", and 
#returns it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
  
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
  
    mat <- x$get()
    inverse <- solve(mat)
    x$setinverse(inverse)
  
    ## Return a matrix that is the inverse of 'x'
    inverse
}