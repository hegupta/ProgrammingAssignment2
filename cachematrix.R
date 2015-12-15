## The functions provided here provide a means of caching the inverse of a given matrix.
## Inverse computation is an expensive operation, and caching the inverse can provide
## significant performance benefits when the inverse of the same matrix is retrieved
## multiple times in the same code flow.

## The makeCacheMatrix function creates a wrapper over the supplied matrix which both
## retains a reference to the original supplied matrix and caches the inverse of this
## matrix. The inverse remains cached as long as the matrix to be inverted is not changed
## by invoking set().
## Code which is operating on a matrix whose inverse needs to be cached should first create
## a wrapper over that matrix using this function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function returns the cached inverse of a matrix, populating the cache first
## in case of the first invocation for a given input. The input supplied to this function
## is not the main matrix object whose inverse needs to be computed; instead it is the wrapper
## created when the main matrix is passed to the makeCacheMatrix function.
## Code which wishes to utilize matrix inverse caching functionality should first create a wrapper
## over the matrix using the makeCacheMatrix function and then call cacheSolve on the wrapper
## every time they need to retrieve the cached inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
