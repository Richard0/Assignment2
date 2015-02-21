## These functions cache the inverse of a matrix. It spares us the time
# of computing the same sort of functions/procedures all over.

## This first function can cache the inverse of an object (in this case
# a matrix).

makeCacheMatrix <- function(x = matrix()) {
  inve1 <- NULL
  set <- function(y) {
    x <<- y
    inve1 <<- NULL
  }
  get <- function() x
  setinve1 <- function(inve2) inve1 <<- inve2
  getinve1 <- function() inve1
  list(set=set, get=get, setinve1=setinve1, getinve1=getinve1)
}


## This last function creates the inverse of the first matrix (see above)
# When the inverse is already calculated then the cacheSolve-function
# wil retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inve1 <- x$getinve1()
  if(!is.null(inve1)) {
    message("getting cached data.")
    return(inve1)
  }
  data <- x$get()
  inve1 <- solve(data)
  x$setinve1(inve1)
  inve1
}


