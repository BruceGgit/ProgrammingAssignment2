## These functions create a matrix object that can cache its inverse
## 

## makeCacheMatrix creates an object that encapsulates a matrix
## and the cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invcache <- NULL
    set <- function(y) {
      # Set the object X to the matrix, and clear the inverse
      x <<- y
      invcache <- NULL
    }
    # Return the matrix
    get <- function() x
    setInverse <- function(inverse) invcache <<- inverse
    getInverse <- function() invcache
    
    ## Return a list of functions as the object
    list(set=set, get=get,
         setInverse=setInverse, 
         getInverse=getInverse)
}


## cacheSolve inverts the matrix (if not already done) and
## stores the inverse in the cache.  If there's already an 
## inverse in the cache, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        # This will invert a square matrix
        # Non-square matrices do not have inverses!
        # (Thank you, Wikipedia)
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
