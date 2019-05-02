## Function to return the cached matrix inverse
## if it is already computed.

## Function to define setter and getter method
## given the matrix x

makeCacheMatrix <- function(x = matrix()) {
  # Initialize variables
  m <- NULL

  # Implement setter and getter functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x

  setsolve <- function(solve) m <<- solve
  getsolve <- function() m

  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function which computes inverse of 'x' if it
## is not computed yet. If it is already computed,
## this returns the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()

  # If the inverse is already computed, return
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)

  m
}