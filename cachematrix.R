## These two functions create a matrix for which the inverse can be cached, and;
## returns the cached inverse if it exists or else computes the inverse and caches it

## Write a short comment describing this function
## Returns a list containing functions that can be used to get or set the original matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Returns the inverse of the matrix passed in via the x parameter, either from cache or by computing it
## Any computed inverse matrix will be cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
