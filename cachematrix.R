## These functions are used for caching the inverse of a matrix.

## The "makeCacheMatrix" function creates a special "matrix"
## object (which is really a list) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The "cacheSolve" function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## the cachesolve retrieve the inverse from the cache.
## Otherwise, it computes the inverse of the matrix and 
## set it in the cache.

cacheSolve <- function(x, ...) {
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
