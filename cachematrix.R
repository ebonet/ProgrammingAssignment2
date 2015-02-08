## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Changes the value
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  
  get <- function() x
  
  # sets the inverse. Ideally there should be some kind of check here to see 
  # if the given inv matrix is trully the inverse of matrix x.
  #In my opinion there 
  setInverse <- function(theInverse) inv <<- theInverse 
  
  # returns the inverse
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  theInv <- x$getInverse()
  
  if(is.null(theInv)) {
    message('caching result of inverse') 
    theInv <- solve(x$get(),...) # computes the inverse of the matrix x, passing additional parameters
    x$setInverse(theInv) # sets the inverse to the CacheMatrix
  }
  
  theInv # returns the inverse
    
}
