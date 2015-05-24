## The following two functions makeCacheMatrix() and cacheSolve() can be
## used to cache the inverse of a matrix. As long as the concerned matrix
## is not changed, its inverse is computed only once and can be retrived
## for later usage.

## The makeCacheMatrix() function takes a matrix as its argument, returns
## a special matrix object that:
## (1) holds the matrix and its inverse (originally being null), and
## (2) generates a list of setters and getters that can be used to cache
##     the matrix and its inverse.
## Although the inverse is null at the first time, we can then compute it
## using the following function cacheSolve(), in which the sub-function
## setinverse() will also be invoked to set the inverse into the special
## matrix object for later usage.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve() function takes a special matrix object produced by
## the above makeCacheMatrix() function and returns the inverse. If the
## inverse has already been calculated, it just retrives it from the
## special matrix object by the getinverse() function; Otherwise, if the
## inverse is null, the function will compute the inverse using the "solve"
## function.
## In this assignment, it is assumed that the matrix supplied is always
## invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}