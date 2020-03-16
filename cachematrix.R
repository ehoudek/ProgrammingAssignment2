## The functions makeCacheMatrix and cacheSolve that work in tandem 
## to return a cache that is an inverse of a specified matrix 

## makeCacheMatrix is a function that creates a matrix object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mtrix <- NULL
        set <- function(y) {
                x <<- y
                mtrix <<- NULL
}
get <- function () x
        setinverse <- function(inverse) mtrix <<- inverse 
        getinverse <- function() mtrix
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the matrix created by 'makeCacheMatrix'. 
## If the inverse has been calculated (and not changed), then 'cacheSolve' will return the inverse from the cache. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        mtrix <- x$getinverse()
        if(!is.null(mtrix)) {
                message("getting cached data")
                return(mtrix)
        }
        data <- x$get()
        mtrix <- solve(data, ...)
        x$setinverse(mtrix)
        mtrix
}

## *************TEST********************************
 testm <- matrix(rnorm(27),3,3)
 testm1 <- makeCacheMatrix(testm)
 cacheSolve(testm1)
