## Matrix inversion is a costly computation and there are some benefits to caching the inverse of a matrix rather than compute it repeatedly. The pair of functions here are used to cache the inverse of a matrix.

## This function creates a specil "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <- NULL
	}
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. If the inverse has been calculated and the matrix remains the same, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.nulll(m)) {
    	message("getting cached data")
    	return(m)
    }
    data <- x$getInverse()
    m <- solve(data, ...)
    x$setInverse(m)
    m 
}
