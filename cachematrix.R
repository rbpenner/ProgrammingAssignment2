## These two functions find the inverse of a given matrix and cache the results
## so the computations do not have to be repeated.

## This function, makeCacheMatrix, accepts as input a matrix--which should be a 
## square matrix--and returns a list containing that matrix along with an empty
## element for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    setmatrix <- function(y) {
        x <<- y
        im <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) im <<- solve
    getinverse <- function() im
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
         getinverse = getinverse)
}

## This function, cacheSolve, returns the inverse of the matrix defined in the
## makeCacheMatrix function. If cacheSolve has not been run before on the
## matrix, it caches the inverse and returns that cached value when called in
## the future.

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    ## Check if the inverse has already been cached.
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$getmatrix()
    im <- solve(data, ...)
    x$setinverse(im)
    ## Return a matrix that is the inverse of 'x'
    im
}
