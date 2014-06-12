## Author: redus
## Desc: Compute and cache the inverse of an invertible square matrix.

## REQ: x is an invertible square matrix
## EFF: set list of fns to inverse and cache in parent env.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) inv <<- inverse
	get_inverse <- function() inv
	list(set = set, get = get, 
		set_inverse = set_inverse, 
		get_inverse = get_inverse)
}


## REQ: x is output of makeCacheMatrix() 
## EFF: return the inverse matrix
cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
	if (!is.null(inv)){
		# message("cached")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$set_inverse(inv)
	inv
}
