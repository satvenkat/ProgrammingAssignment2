## The purpose of this is to help us understand the concepts of objects
## as functions (similar to javabeans in java) to store values,
## and using another function to set the values in the javabean like objects
## In this case, it is used to store the matrix and the inverse of the matrix

## This function has getters and setters for a given matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## This function is meant to return a matrix's inverse.
## It uses the object x's methods. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}