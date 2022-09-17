## Put comments here that give an overall description of what your
## functions do

## This function creates a special 'matrix' that has its inverse computed and cached in an environment different than the one where the function is defined.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of a matrix if its already not done and if it is, fetches the value of the inverse from the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached value")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
