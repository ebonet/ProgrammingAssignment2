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
  setInverse <- function(theInverse) inv <<- theInverse 
  
  # returns the inverse
  # In my opinion there should be no cacheSolve, the check for null should 
  # happen here, as well as calculating the inverse and caching.
  getInverse <- function() inv
  
  # returns the list with pointers to the functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

## Computes the inverse of a CacheMatrix and returns it
cacheSolve <- function(x, ...) {
  theInv <- x$getInverse()
  
  if(is.null(theInv)) { # cache is empty
    message('caching result of inverse') 
    theInv <- solve(x$get(),...) # computes the inverse of the matrix x, passing additional parameters
    x$setInverse(theInv) # sets the inverse to the CacheMatrix
  }
  
  theInv # returns the inverse
    
}
