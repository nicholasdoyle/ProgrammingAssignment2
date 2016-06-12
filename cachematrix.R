## These functions will assist with calculating inverses of matrices, and reduce the cost through caching

## This function creates a cache matrix object, which will support caching the inverse in the closure

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will take a matrix object, created by makeCacheMatrix, and calculate the inverse.  
## It will leverage the caching functionality provided by the matrix object.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cahced data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
