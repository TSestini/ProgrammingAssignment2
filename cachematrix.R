## This pair of functions can be used to cache the inverse of a matrix so it can
## be looked up later, instead of having to compute repeatedly.


# The makeCacheMatrix function creates a special 'matrix' that can cache its
# inverse.

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

# The cacheSolve function computes the inverse of the matrix which is returned
# from the makeCacheMatrix function. If the inverse has already been calculated,
# the cacheSolve can retrieve it from the cache instead of recalculating it.

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
