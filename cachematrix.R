## From the makeVector example:
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invert <<- solve
        getinverse <- function() invert
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## From the cachemean example:
## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
        invert <- x$getinverse()
	if(!is.null(invert)) {
  		message("getting cached data")
	  	return(invert)
    	}
	temp <- x$get()
	invert<- solve(temp, ...)
	x$setinv(invert)
	invert
}
