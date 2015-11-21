## Matrix inversion often takes long and it will be beneficial to cashing
## a inverse of a matrix rather than computed repeatedly.  This file will
## includes two functions.

## FUNCTION 1 - This will create a special "matrix" object that can cache its
## inverse.  This function will includes 4 functions.
## (1.Set the matrix,           2.Get the matrix,
##  3.set the inverse matrix,   4.get the inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function 2 - This will compute inverse of the special "matrix" returned by
## above function.  If the inverse has already been calculated (and the matrix
## has not changed), the value will obtain from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
