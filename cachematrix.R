## Below are two functions that create an object which stores a matrix and cache's its inverse.
## To calculate the inverse, first the inverse of the matrix is checked if it has already been calculated or not in the cache.
##In this way, repeated calculations can be skipped.

## This function sets the value of a matrix and gets its value. Then it sets the inverse of that matrix and gets it.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
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


## The following function checks if the inverse of the matrix is already in the cache or not.
## If its there, the function gets the inverse from cache. If not, it calculates it and sets the inverse and prints it.

cacheSolve <- function(x, ...) {
         inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
