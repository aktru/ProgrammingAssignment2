## This set of functions cache the inverse of a matrix.
##
## Example of using:
## > t <- rbind(c(4,5), c(5,4))
## > tl <- makeCacheMatrix(t)
## > cacheSolve(tl)
##           [,1]       [,2]
## [1,] -0.4444444  0.5555556
## [2,]  0.5555556 -0.4444444


## Function makeCacheMatrix creates special list containing 
## functions for setting and getting value of cache

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL				## defining variable m in the local environment
	set <- function(y = NULL) {		## defining function of set the value of the matrix
		x <<- y				## defining variable x in the environment where the function was called 
		m <<- NULL			## similarly for variable m 
	}
	get <- function() x			## defining function of get the value of the matrix
	setinverse <- function(solve) m <<- solve ## defining function of set the value of the inverse of a matrix
	getinverse <- function() m		## defining function of get the value of the inverse of a matrix
	list (set = set, get = get, 		## defining list containing functions
		setinverse = setinverse,
		getinverse = getinverse)
}


## Function cacheSolve checks existence of cache and returns value 
## from cache if it exists. If cache isn't exists then function 
## computes the inverse of a matrix and returns resulting value. 

cacheSolve <- function(x, ...) {
	m <- x$getinverse()			## getting value from cache
        if(!is.null(m)) {     			## if value in cache isn't NULL
        	message("getting cached data")
        	return(m)		  	## returning value from cache and finishing of the function
        }
        					## if value in cache is NULL then
        data <- x$get()       			## getting a matrix for inverse
        m <- solve(data)      			## inversing of a matrix
        x$setinverse(m)       			## setting value to cache
        m					## returning value and finishing of the function
}
