## The following functions enable you to save the inverse of a matrix
## Create the object for caching by calling makeCacheMatrix and get the inverse by calling cacheSolve

## makeCacheMatrix creates the object that contains the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix x
## The argument for calling this function is the object returned by makeCacheMatrix
## If the inverse has already been calculated and there was no change to the matrix, it will be read from the cache
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
