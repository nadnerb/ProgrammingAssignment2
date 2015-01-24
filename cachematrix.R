## The two functions cache the inverse of a given matrix.
##
## Note: the matrix supplied is always invertible.

## makeCacheMatrix is a function taking a matrix as it argument.
##
## It will expose:
##
## Setting the value of the matrix
## Getting the value of the matrix
## Setting the inverse of the matrix
## Getting the inverse of the matrix

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

## Return the inverse of the makeCacheMatrix function. 
## This will return the cached inverse if it already has been calculated 
## and a message will be displayed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
