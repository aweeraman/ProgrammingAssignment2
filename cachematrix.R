
## Creates a special matrix object capable of caching its inverse. This
## function implements functions set(), get(), setinverse() and getinverse().

makeCacheMatrix <- function(matrix = matrix()) {
  
  # stores the cached value of the matrix
  cache <- NULL
  
  # stores the actual matrix
  set <- function(m) {
    matrix <<- m
    cache <<- NULL
  }
  
  # returns the stored matrix
  get <- function() matrix
  
  # caches the inverse of a matrix
  setinverse <- function(solve) cache <<- solve
  
  # returns the cached inverse of a matrix
  getinverse <- function() cache
  
  # returns a list where each element is a function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the "special" matrix
## created using makeCacheMatrix and attempts to compute the inverse.
## If the inverse has already been computed, it returns it from the
## cache instead of re-computing it.

cacheSolve <- function(matrix, ...) {
  
  # attempts to retrieve the cache of the inverse of a matrix
  inverse <- matrix$getinverse()
  
  # if a cache value is present, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if not, get the value of the matrix, compute the inverse
  data <- matrix$get()
  inverse <- solve(data)
  
  # cache the computed version of the inverse
  matrix$setinverse(inverse)
  
  # return the inverse
  inverse
}
