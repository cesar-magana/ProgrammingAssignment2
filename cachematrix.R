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
		set <- function(y) {
		    x <<- y
		    inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(	set = set, 
				get = get, 
				setinv = setinv, 
				getinv = getinv )
}
# cacheSolve: 
# Calculate the inverse of the given matrix.
# It returns the cached inverse if is already calculated before.
# Description
# 1. If the inverse is already calculated, return it
# 2. The inverse is not yet calculated, so we calculate it
# 3. Cache the inverse
# 4. Return it
cacheSolve <- function(x, ...) {
	    ## Return a matrix that is the inverse of 'x'
	    inv <- x$getinv()
	    if (!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
	    }
	    data <- x$get()
	    inv <- solve(data, ...)
	    x$setinv(inv)
	    inv
}