## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) iv <<- solve
    getinverse <- function() iv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data)
    x$setinverse(iv)
    iv
}
