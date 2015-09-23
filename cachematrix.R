## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function generates a special matrix, which caches the inverse
## of it. 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
}
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

## Write a short comment describing this function
## This function calculates the inverse of the non-singular special
## matrix defined in makeCahceMatrix. If the cached result is valid,
## it will be returned as such without recalculation. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
