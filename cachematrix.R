## These functions provide the data structure and operations for a cached version
## of a matrix.
##
##  Inverse - This is the only operation at this time. 

## This function will create a cached version of a matrix using the <<- operator.
## Matrix retrievals will be much faster using this cached version.

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


## Calculates the inverse of a Matrix using a cached version of matrix 'x'.
## The matrix inversion is calculated using the R internal 'solve' function. 
## This function assumes that the input Matrix is square and invertible.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
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
