### R programming: Programming assignment 2 - Catching the inverse of a matrix###

## As a way to make the inversion of a matrix more efficient, the next couple of functions 
## store the result of this computation directly in the cache for subsequent fast recovery.

## makeCacheMatrix creates a list containing four elements is: the first element sets the 
## values of a square invertible matrix, the next one gets these values, the third one sets
## the inverse of the matrix, while the last one stores this inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y = matrix){		## The operator <<- assigns y to x, which means	
		x <<- y				## that if this variable is found in the parent 
		m <<- NULL				## environment it will be assigned; otherwise, 
	}				     ## the assigment takes place in the global environment.
	get <- function() x				
	setInverse <- function(solve) m <<- solve		
	getInverse <- function() m				
	list(set = set, get = get, 				
		setInverse = setInverse, 
		getInverse = getInverse)
}


## The cacheSolve function verifies whether the inverse matrix of the matrix created with 
## makeCacheMatrix has been calculated, in which case it will return a message indicating
## that is was recovered from the cache along with the value, without the need of doing it
## again. If this is not the case, the function will calculate the inverse with the 'solve'
## function and it will set the value in the cache for further usage.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)){			## The condition will look if there is already a value.
		message("Getting cached data")
		return(m)
	}
	data <- x$get()        ## Otherwise, it will return a matrix that is the inverse of x.
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
