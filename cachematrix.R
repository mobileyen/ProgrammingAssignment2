## Caching the Inverse of a Matrix:
## Because matrix inversion requires costly computation,
## these functions allow the caller to compute the matrix
## inverse once and cache the results for future use,
## avoiding costly recomputation.

## makeCacheMatrix: creates a special matrix object that
##   can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: computes inverse of the matrix object made
##   by makeCacheMatrix, returning cached version if it has
##   been computed previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return i
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
