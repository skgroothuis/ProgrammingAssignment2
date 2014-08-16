#
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve returns inverse of a matrix A created 
## with makeCacheMatrix function.

## If cached inverse is available, cacheSolve retrieves it.
## If cached inverse is unavailable, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix...")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}

## Sample run:
## > x = rbind(c(2, 1/2), c(1/3, 3))
## > m = makeCacheMatrix(x)
## > m$get()
##          [,1] [,2]
## [1,] 2.0000000  0.5
## [2,] 0.3333333  3.0
##
## No cache in the first run
## > cacheSolve(m)
##          [,1]  [,2]
## [1,] 2.0000000  0.5
## [2,] 0.3333333  3.0
##
## Retrieving from the cache in the second run
##
## > cacheSolve(m)
##
## getting cached inverse matrix...
##             [,1]        [,2]
## [1,]  0.51428571 -0.08571429
## [2,] -0.05714286  0.34285714
##
