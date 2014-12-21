## The following functions enable you to save the inverse of a matrix
## Create the object for caching by calling makeCacheMatrix and get the inverse by calling cacheSolve

## makeCacheMatrix creates the object that contains the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    matInverse <- NULL
    set <- function(y) {
        x <<- y
        matInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) matInverse <<- solve
    getInverse <- function() matInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix x
## The argument for calling this function is the object returned by makeCacheMatrix
## If the inverse has already been calculated and there was no change to the matrix, the inverse will be read from the cache
cacheSolve <- function(x, ...) {
    matInverse <- x$getInverse()
    #Check if the inverse of the matrix was already set
    if(!is.null(matInverse)) {
        message("getting cached data")
        return(matInverse)
    }
    #if the inverse was not set, then calculate the inverse and write it to the cache
    data <- x$get()
    matInverse <- solve(data, ...)
    x$setInverse(matInverse)
    matInverse
}
