## makeCacheMatrix() and cacheSolve() are used to compute the inverse of
## an n-by-n matrix once, then store it in a cache for later reference,
## if the original matrix has not changed.

## makeCacheMatrix() assigns a set of four functions to get/set the original
## matrix or get/set the inverse of the original matrix hence:
## > t<-makeCacheMatrix() ## assigns the functions to t
## > t$set(matrix(c(0,5,5,0),2,2)) ## sets the original matrix in t
## > t$get() ## will display the original matrix
##      [,1] [,2]
## [1,]    0    5
## [2,]    5    0
## attempting to set a non-n-by-n matix will result in the message:
## "This is not an nXn invertible matrix. Input is ignored!!!"


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		if (ncol(y)==nrow(y)) {
			x <<- y
			m <<- NULL
		} else {
			message("This is not an nXn invertible matrix.")
			message("Input is ignored!!!")
		}
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve() is a function that computes the inverse of the original
## matrix stored (see above), or return the cached result from an earlier
## call, thus:
## > cacheSolve(t) ## computes the inverse of the original matrix in t
##      [,1] [,2]
## [1,]  0.0  0.2
## [2,]  0.2  0.0
## Calling cacheSolve(t) again, without changing the original matrix in t
## will display the message "Cached data used" before returning the cached
## result.


cacheSolve <- function(x, ...) {
	 ## Return a matrix that is the inverse of 'x'
	 m <- x$getInverse()
	if(!is.null(m)) {
		message("cached data used")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m

}
