## Caching the inverse of a matrix
## Save time to avoid compute repeatly

## The first function, makeCacheMatrix creates a special "matrix", which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
  }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated, if so, it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
 	if (!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  }
  	mat <- x$get()
  	inv <- solve(mat, ...)
  	x$setInverse(inv)
  	inv


}
