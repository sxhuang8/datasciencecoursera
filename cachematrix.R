## R Programming Assignment 2
## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Define cache inv 
  set <- function(y) {
    x <<- y 
    inv <<- NULL ## Use superassignment <<- to update the cache in the containing environment
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse ## Use superassignment <<- to update the cache
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ## retrive inverse matrix if it has already been calculated
  } 
  data <- x$get()
  inv <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setinverse(inv) ## The new inverse matrix is stored in cache after calling the function setinverse
  inv
}
