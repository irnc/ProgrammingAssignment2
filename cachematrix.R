## `makeCacheMatrix` and `cacheSolve` work in pair to compute inverse matrix,
## multiple calls on `cacheSolve` for the same matrix would use cache.
##
## Usage example:
##
## test <- matrix(1:4, 2, 2)
## cacheable <- makeCacheMatrix(test)
## cacheSolve(cacheable)
## cacheSolve(cacheable) -- will print "getting cached data" message

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x

  setinverse <- function(cache) inverse <<- cache
  getinverse <- function() inverse

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
##
## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` defined above. Please see usage example above.
##
## If the inverse has already been calculated (and the matrix
## has not changed), then the `cacheSolve` will retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)

  inverse
}
