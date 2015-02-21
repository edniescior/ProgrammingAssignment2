## Calculating the inverse of a matrix is an expensive computation. These 
## functions allow for the result to be cached. Subsequent calls would
## use the cached result rather than having to calculate the inverse again.

## This function creates a special matrix that can cache its inverse. It provides
## getter and setter methods to access the matrix value and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    getinverse <- function() m
    setinverse <- function(inv) m <<- inv
    list(set = set, get = get, 
         getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix.
## If this value has already been calculated (and the matrix has not changed), 
## then the cached value is returned; Otherwise, the inverse is computed, 
## cached and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    v <- x$getinverse()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v
}
