## The two functions in this document, when used together, calculate 
## the inverse of a matrix and cache that value, so it can be retrieved
## the next time the function is run.

## The intent of the 'makeCacheMatrix' function is to create a matrix 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solvematrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
    solvematrix = solvematrix,
    getmatrix = getmatrix)
}


## The intent of the 'cacheSolve' function is to calculate the 
## inverse of the matrix object returned by the 'makeCacheMatrix function.
## If this has already happened, 'cacheSolve' retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$solvematrix(m)
  m
}