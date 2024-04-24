## Put comments here that give an overall description of what your
## functions do

##      The following pair of functions implements a "cached matrix"
##      analogous to the "cached vector" example given in the project's
##      "README.md" file. The cached matrix is created using the first
##      function, makeCacheMatrix() and its inverse is obtained using
##      the second function, cacheSolve().

##      Example usage (from the R prompt):
##          > z <- makeCacheMatrix(matrix(c(2,0,0,2), 2, 2))
##          > z_inv <- cacheSolve(z)

## Write a short comment describing this function

##      makeCacheMatrix() creates the "cached matrix" as a list of four
##      functions that set and get the values of the underlying matrix
##      and its inverse. The supplied matrix argument, x, is assumed to
##      be square and non-singular.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) x_inverse <<- inverse
        getinverse <- function() x_inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

##      cacheSolve() returns the inverse of a "cached matrix" previously
##      created using makeCacheMatrix(). The inverse is actually only
##      computed on the initial call; subsequent calls return the cached
##      inverse. The underlying matrix is assumed to be square and also
##      non-singular. No error-checking is performed. However, a message
##      is printed if the cached inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
