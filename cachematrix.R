## PROGRAMMING ASSIGNMENT 2

## This function creates a special "matrix" object that can cache its inverse.
## Setting and getting the values of the matrix
## Setting and getting the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## checks if the inverse is already calculated and returns it.
## else the inverse is calculated and assigned.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
