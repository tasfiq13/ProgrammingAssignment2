## Setting directory

setwd('F:/R/Coursera/Course1/week3')
##
## x is a matrix
## Function for "makeCacheMatrix" and "cacheSolve" to cache the inverse of a matrix

## "makeCacheMatrix" is a function for creating "matrix"
## It can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##
## "cacheSolve" function for retrieveinf the cache

cacheSolve <- function(x, ...) {  
  inv <- x$getinv() ## Retun a inverse matrix of 'x'
  if(!is.null(inv)) {
    message("Getting the cahched data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
