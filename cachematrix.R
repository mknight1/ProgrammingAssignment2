## These funcitons cache the inverse of a matrix to avoid its computation
## in multiple repeated calls.  The solve function is used to compute the
## matrix inverse.

## This function creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the makeCacheMatrix function result.  
## If the inverse has been calculated and the matrix hasn't changed, then the 
## inverse should be retrieved from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}