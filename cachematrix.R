## These set of functions provide a way to repeatedly and performantly
## calculate the inverse of a matrix, by caching (or memoizing) the results.
##
## Usage:
##
## Create the matrix
## mat <- matrix( c( 2,2, 3,2 ), nrow = 2, ncol = 2)
##
## Create the cached matrix data structure
## cachedMatrix <- makeCacheMatrix(mat)
##
## Somewhere, perhaps in a loop
## inverse <- cacheSolve(cachedMatrix)

## Create a datastructure (a list) that holds a matrix and internally,
## a cached value of the inverse of the matrix.
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(mtrx) {
        matrix <<- mtrx
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function accepts the data structure created by makeCacheMatrix, and
## returns its inverse. The inverse is first calculated if it isn't already
## cached and the cached value is returned in subsequent invokations on the
## same list.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## no cached inverse found, calculate the inverse
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    ## store the result in cache
    x$setinverse(inverse)
    inverse
}
