## Create a special cache matrix via makeCacheMatrix that can cache the inverse of itself which
## can be retrieved via cacheSolve()

## Create a special cache matrix from matrix x
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL   # cached inverse of matrix x
    set <- function(y) {
        x <<- y
        inv_x <<- NULL  # reset to NULL
    }
    get <- function() x
    setinverse <- function(inv) inv_x <<- inv
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Try to fetch the cached inverse of matrix x and return it. If not available, calculate
## the inverse of x, store it in the cache for next use and return it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {     # cached inverse exists -> return it
        message("getting cached data")
        return(inv)
    }
    
    # cached inverse does not exist -> calculate it, store it, return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
