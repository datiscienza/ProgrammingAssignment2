## Allows the creation and usage of special matrix objects that
## can cache their inverse.

## Creates a special "matrix" object that can cache its inverse.
## Clients of makeCacheMatrix can set() an ordinary matrix,
## and retrieve it later with get()
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = sestinverse, getinverse = getinverse)
}


## Calculates the inverse of a special "matrix" created with
## makeCacheMatrix(). Calculation is done once and subsequently
## looked up from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) return(inverse)
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
