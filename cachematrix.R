## The first function 'makeCacheMatrix' creates a special 'matrix' object which can cache its 
## inverse.  The inverse is calculated using the 'solve' function and assumes that the matrix
## entered is square and invertible.


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


## The second function 'cacheSolve' computes the inverse of the special matrix returned by 
## makeCacheMatrix.  If the inverse has already been calculated, the function retrieves the 
## value from the cache instead of repeating the calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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