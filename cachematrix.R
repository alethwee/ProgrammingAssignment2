## The two functions create a special object that stores a matrix and
## caches its inverse. The <<- operator is used to assign values to the matrix
## in an environment that is different from the current environment.

## makeCacheMatrix creates a special "matrix" object which stores two values:
## the original value and what will be the cached value, which is initially set 
## to 'NULL'. To do this, it contains four functions: two to 'get' the values 
## and another two to 'set' them.  

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 

## cacheSolve calculates the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setmean(i)
    i
}