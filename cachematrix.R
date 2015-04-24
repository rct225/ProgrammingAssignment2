## These functions create a special class of "matrix" that
## has the ability to cache a copy of it's inverse
## 
## Caching the calculation can be useful in situations where 
## there may be numerous manipulations of a matrix 

## This function creates an object that will store a matrix
## and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		
		set <- function(y) {
		    x <<- y
		    inv <<- NULL
		}
		get <- function() x
		setInverse <- function(solve) inv <<- solve
		getInverse <- function() inv
		list(set = set, get = get, 
		     setInverse = setInverse,
		     getInverse = getInverse)
}


## This function checks to see if the inverse for the matrix
## has been already calculated.
##
## If the inverse has not been calculated, the function calculates the
## inverse using the solve() function.  
##
## Note: only works on invertible matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
	   message("getting cached data")
	   return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
