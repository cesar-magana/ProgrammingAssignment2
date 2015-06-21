# Example usage:
# > A <- matrix(rnorm(36), nrow = 6)          # Create a matrix A.
# > CachedA <- makeCacheMatrix(A)             # Create a special matrix.
# > CachedA$get()                             # Return the matrix.
# > cacheSolve(CachedA)                       # Return the inverse
# > cacheSolve(CachedA)                       # The second time return the cached inverse 

# makeCacheMatrix: 
# Returns a list of functions:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		# Set the value of the matrix
		set <- function(y) {
		    x <<- y
		    inv <<- NULL
		}
		#Get the value of the matrix
		get <- function() x
		#Set the value of the inverse
		setinv <- function(inverse) inv <<- inverse
		# Get the value of the inverse
		getinv <- function() inv
		list(	set = set, 
				get = get, 
				setinv = setinv, 
				getinv = getinv )
}
# cacheSolve: 
# Description
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

# 1. If the inverse is already calculated, return it
# 2. The inverse is not yet calculated, so we calculate it
# 3. Cache the inverse
# 4. Return it
cacheSolve <- function(x, ...) {
	    ## Return a matrix that is the inverse of 'x'
	    # If the inverse is already calculated, return it
	    inv <- x$getinv()
	    if (!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
	    }
	    data <- x$get()
	    # The inverse is not yet calculated, so we calculate it
	    inv <- solve(data, ...)
	    # Cache the inverse
	    x$setinv(inv)
	    # Return it
	    inv
}
