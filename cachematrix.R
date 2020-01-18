## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  
  #set the value of the matrix
  set <- function(y) {
      ## assign 'y' value to the matrix 'x'
      x <<- y
      m <<- NULL
  }
  
  #get value of the matrix
  get <- function() x
  
  #set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  #get the inverse of the matrix
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function checks if the matrix inverse has been already calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imatrix = x$getinverse()
        
        if(!is.null(imatrix)) {
          message("Getting cached data.")
          return(imatrix)
        }
        
        data <- x$get()
        imatrix <- solve(data, ...)
        x$setinverse(imatrix)
        
        imatrix
}
