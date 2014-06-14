## To reduce the computational time of repeated inversion 
## of matrix, it is useful to cache the inverse of the matrix.
## The follwing two functions achieves that, first one creates
## a special cacheable matrix, while the next one finds the
## cached inverse


## Function 1 :This function creates a special "matrix" object 
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve)m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function 2: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
