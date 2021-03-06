## This file contains two functions makeCacheMatrix and cacheSolve
## The aim is to compute the inverse of a non-singular matrix ONLY
## when necessary. If the contents of the inverse are unchanged, it
## will get the value from the cache rather than recompute the inverse.
## It will save computing time when it has to be computed
## repeatedly (e.g. in a loop). 

## Suppose A is non-singular matrix. x<-makeCacheMatrix(A) will generate
## a list x containing 4 functions that will be used in cacheSolve.
## cacheSolve(x) will return the inverse of A from the cache rather
## than recompute the inverse of A.



## This function take a non-singular matrix as input argument,
## and output a list containing four functions:
## set: set the value of the matrix
## get: get the value of the matrix
## setsolve: set the value of the inverse matrix
## getsolve: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	# create a list of 4 functions: set, get, setsolve, getsolve

	# initialize m to NULL
	m <- NULL
	
	# create the set function to set the value of y to x and m to NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	# create the get function to get value to x
	get <- function() x
	
	# create the solve function to compute the inverse and store in m
	setsolve <- function(solve) m <<- solve
	
	# create the getsolve function to get the value in m
	getsolve <- function() m
	
	# create a list containing these 4 functions as output
	list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function compute the inverse of a non-singular matrix,
## uses the functions in makeCacheMatrix to check if
## the contents of the inverse matrix are unchanged, it only looked up 
## the value from the cache rather than recompute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the inverse and store in m
	m<-x$getsolve()
	
	# if m is not NULL, get the value from cache and return
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# get the data and compute the inverse by solve
	data <- x$get()
	m <- solve(data,...)
	
	# set the inverse to m and output
	x$setsolve(m)
	m
}
