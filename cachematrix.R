## There are two functions in this file.
##  - The first one makes a list of function with
##    different read/write behaviour
##  - The second one calculates the inverse of a matrix
##    if its value is not stored in the cache, and saves it. 
##    If the value is already in the cache, just reads it  
##    and returns it.

## This function creates a list of functions which read/write 
## two different values from/in the global environment instead 
## of the function environment

## It is useful for storing the values of the matrix and its inverse
## the first time they are calculated and for getting them back from 
## the cache if the value is already there

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of a matrix and can do it through 
## two different ways:

##  - If the inverse of the matrix is already stored in the cache, 
##      just reads it and returns it.
##  - If the inverse is not there, reads the matrix, calculates its
##      inverse using the "solve()" function, stores the inverse in
##      the cache, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
