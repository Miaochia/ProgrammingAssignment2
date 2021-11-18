## To create a special object that stores a matrix and caches its inverse.

## The following function creates a special "vector", which is really a list
## containing a function to set the value of a matrix, get the value of the
## matrix, set the value of the inverse of the matrix, and get the value of the
## inverse of the matrix.

makeCacheMatrix <- function(a, b, c) {
  x <- matrix(a, b, c)
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function generates the inverse of the matrix created with the
## above function. However, it first checks to see if it has already been
## generated. If so, it gets the value from the cache and skips the computation.
## Otherwise, it generates the inverse of the matrix and stores it in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
