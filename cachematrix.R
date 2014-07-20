## These two functions work together to cache the inverse of a matrix 

## This first function caches the inverse of a matrix

makeCacheMatrix <- function(M = matrix()) {
	Inverse <- NULL
	set_matrix <- function(x) {
		M <<- x
	  Inverse <<- NULL
	}
	get_matrix <- function() M
	set_inverse <- function(solve) Inverse <<- solve
	get_inverse <- function() Inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}



## This second function computes the inverse of the matrix
## returned by the function "makeCacheMatrix".  If this
## matrix has not changed and the inverse is already
## calculated, then the function uses the cached version 
## of the inverse

        
cacheSolve <- function(M, ...) {

        ## Return a matrix that is the inverse of 'M'

	Inverse <- M$get_inverse()
	if(!is.null(Inverse)) {
		message("getting cached data")
		return(Inverse)
	}
	data <- M$get_matrix()
	Inverse <- solve(data, ...)
	M$set_inverse(Inverse)
	Inverse
}

