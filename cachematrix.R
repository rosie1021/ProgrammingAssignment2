## This is an R function that is able to cache the inverse of a matrix to avoid costly computation

## The first function, makeCacheMatrix creates a special "matrix", which is a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get =get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
