## Setting up a matrix which can cache its inverse.
## Step 1 - Set the Matrix
## Step 2 - Get the Matrix
## Step 3 - Set the inverse of the Matrix
## Step 4 - Get the inverse of the Matrix
## makeCacheMatrix will build a list for cacheSolve
makeCacheMatrix <- function(x = matrix()) {
	inv_m = NULL  
	set = function(y) {
		x <<- y 
		inv_m <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv_m <<- inverse
	getinv = function() inv_m
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Compute the inverse
## Step 1 - Look at the Inverse
## Step 2 - Check to see if it has been calculated and not changed
## Step 3 - Retrieve the inverse from he cache
cacheSolve <- function(x, ...) {
	inv_m = x$getinv()
	if (!is.null(inv_m)) {
		message("getting cached data")
		return(inv_m)
		}
	data = x$get()
	inv_m = solve(data,...)
	x$setinv(inv_m)
	return(inv_m)
}
