## This functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## This object is the list of functions, which allow to store data in cache and get them.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes as an argument special "matrix" object, created by previous
## function, and return inverse of this matrix. If inverse of matrix has already been 
## computed, function get it from the cache.

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
