## These functions use a cache to save computed inverse matrix of the previously created one.

## makeCacheMatrix creates a matrix (square matrix)
## and then calculate the inverse matrix to the created one.
## The inverse matrix is then saved to a cache.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks if the inverse matrix has been already computed.
## If yes it returns the matrix from cache.
## If not it will compute the new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setinverse(m)
	m
}
