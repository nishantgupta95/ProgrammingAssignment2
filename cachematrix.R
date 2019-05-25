## Both functions cache the inverse of the matrix and calculates the inverse of matrix in case of
## matrix doesn't changed.

## Creates A Special Matrix Object that can cache its Inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invrse) inv <<- invrse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes Inverse of The special Matrix Object returned by makeCacheMatrix Function.
## If the inverse has already been calculated , then the cacheSolve should retrieve the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
