## This file contains two functions, that together, create a functionality for taking the inverse of a matrix while caching the result.
## when the inverse is taken again using the same method, the cached value is returned to save the processing time of recalculating it.

## - makeCacheMatrix, is a function that takes a matrix argument,
## and returns an object that contains the matrix and the necessary functions for cacheing the inverse.
## makeCacheMatrix() takes a singe input, the matrix to be inverted. It returns a list that contains
## functions for retrieving the data (get()), setting the data to be a different matrix (set()), retreiving the inverse
## of the matrix (getinv()) and storing the inverse of the matrix in the cache once it's calculated (setinv())

##  - cacheSolve() is the function that can be used with an object created with makeCacheMatrix. For a matrix, "z" that needs
## to be inverted. The makeCacheMatrix object has first been run to create, say, "zc" ("> zc<-makeCacheMatrix(z)").
## cacheSolve() takes such an object (ie "zc") created by makeCacheMatrix to represent the matrix that needs to be inverted.
## cacheSolve() solves the inverse of the matrix, returning the inverse. The first time it is run, it calculates the inverse and
## caches it. Further runs will just return the cache value, saving processor time, and display a message saying it did so.


## This function creates the matrix list object from a matrix argument, with the output containing the matrix data and functions for cacheing.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invers) inv <<- invers
	getinv <- function() inv
	list(get = get, setinv = setinv, getinv = getinv)
}


## This function takes the matrix list object as an argument and returns the inverse. If it has taken the inverse before, it returns the cached
## value instead of recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("retrieving previously computed and cached matrix inverse")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
