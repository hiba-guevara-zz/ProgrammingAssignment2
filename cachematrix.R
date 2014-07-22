## cachematrix is a two-step solution to save the inverse of any given matrix to be used
## anywhere in the working environment without the need to calculate it whenever it's needed.


## makeCacheMatrix returns a list of functions, to be applied later (in cacheSolve) to create
##      and set/save the inverse of a given matrix for easy access anywhere in the current environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    w <<- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns the inverse of w(outcome of makeCacheMatrix), either by "getting" it
##      in case it was already calculated.
## Otherwise, it does the calculation, and sets it for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}