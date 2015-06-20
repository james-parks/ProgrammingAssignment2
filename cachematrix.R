## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix is a function that stores a list of functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
##set is a function that changes the matrix stored in the main function.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## get is a function that returns the matrix x stored in the main function.
        get <- function() x
## setinverse and getinverse don't calculate inverses they store the values in m, 
        setinverse <- function(theinverse) m <<- theinverse
        getinverse <- function() m
## the following line stores the four functions:
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
## cacheSolve first verifies the value m, stored previously with getinverse, exists and is not NULL. 
## If it exists in memory, then it returns the message and the value m
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## data gets the matrix stored with makeCacheMatrix
        data <- x$get()
## m calculates the inverse of the matrix 
        m <- solve(data, ...)
## x$setinverse(m) stores it in the object generated that was assigned with makeCacheMatrix.
        x$setinverse(m)
        m
}


xxx <- matrix(
+ c(2,4,3,1),
+ nrow=2,
+ ncol=2)

