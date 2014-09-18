## version by dmksbk@gmail.com

## makeCacheMatrix creates version of the matrix
## wich caches it's inversevalue for purpose of later reuse

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv))
        {
            message("Getting cached verion of inverse d matrix")
            return(inv)
        } else {
            message("Calculating inverse of a matrix")
            m <- x$get()
            inv <- solve(m)
            x$setinverse(inv)
            return(inv)
        }
}
