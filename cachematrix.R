## version by dmksbk@gmail.com

## makeCacheMatrix creates version of the matrix
## wich caches it's inversevalue for purpose of later reuse

## Creates cached version of a matrix. x is an ordinary matrix value
# that will be assigned to its cached version
makeCacheMatrix <- function(x = matrix()) {
    # Cached value of inverse matrix
    inv <- NULL
    
    # Setting value of a matrix
    set <- function(val) {
        x <<- val
        inv <<- NULL
    }
    
    # Getting value of a matrix
    get <- function() x
    
    # Set value of inverse matrix
    setinverse <- function(i) inv <<- i
    
    # Get value of inverse matrix
    getinverse <- function() inv
    
    # Returning a list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Analog of solve function that uses cached version of a matrix as it's first argument

cacheSolve <- function(x, ...) {
    # Get current version of inverse matrix
    inv <- x$getinverse()
    # If it's not null - we already have this value. Just return it
    if (!is.null(inv))
    {
        message("Getting cached verion of inverse d matrix")
        return(inv)
    } else {
        # Otherwise, if this value is null - inverse matrix has not been calculated yet.
        # We have to calculate it and return
        message("Calculating inverse of a matrix")
        m <- x$get()
        inv <- solve(m)
        x$setinverse(inv)
        return(inv)
    }
}
