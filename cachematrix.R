#
# makeCacheMatrix: 
# This function consists of four sub functions to create a special "matrix" object that can cache its inverse.
# 1. set(y = matrix()): set the value of the matrix
# 2. get(): return the matrix
# 3. setInverse(inverse = matrix()): set the inverse of the matrix for caching
# 4. getInverse(): get the inverse of the matrix
#
# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the inverse is returned from the cache.

#
#  Creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#
# Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# Return the inverse value from the cache when it has been calculated
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  m <- x$get()
  inverse <- solve(m, ...)
  x$setInverse(inverse)
  inverse
}
