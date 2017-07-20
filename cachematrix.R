## below are two functions that are used to create a special object that stores a matrix
## and caches its inverse.

makeCacheMatrix <- function(x = matrix()) { ## initialises x
	n <- NULL                               ## initialises n
	set <- function(y) { 
	x <<- y                                 ## assigns input argument to the x object in the parent environment
	n <<- NULL                              ## assign the value of NULL to n object in the parent environment
  }
	get <- function() x                     ## defines getter for the matrix x
	setinverse <- function(inverse) n <<- inverse  ## defines setter for the inverse n
	getinverse <- function() n                     ## defines getter for the inverse n
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)            ## assign each of these four functions as an element within a 
											## list(), and each element within the list is named
}

cacheSolve <- function(x, ...) {
	n <- x$getinverse()           ## retrieves an inverse from the object passed in the function as an argument
	if(!is.null(n)) {  			  ## check to see if the result is NULL
    message("getting cached data")
    return(n)
  }
  data <- x$get()  				  ## if !is.null(n) is FALSE, cacheSolve() gets the vector from the input object
  								  ## and calculate an inverse by the solve() function
  n <- solve(data, ...)
  x$setinverse(n)
  n								  ## Return a matrix that is the inverse of 'x'
}