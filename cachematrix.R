    ## This function creates a special matrix for 
    ## checking if the inverse has been calculated or not.
	makeCacheMatrix <- function(x = matrix()) {
	        m <- NULL
	        set <- function(y) {
	                x <<- y
	                m <<- NULL
	        }
	        get <- function() x
	        setinverse<- function(inverse) m <<- inverse
	        getinverse <- function() m
	        list(set = set, get = get,
	             setinverse = setinverse,
	             getinverse = getinverse)
	        
	}
	

	## This function returns the inverse of the given matrix
	cacheSolve <- function(x, ...) {
	        m <- x$getinverse()
	        if(!is.null(m)) {
	                message("getting cached data")
	                return(m)
	        }
	        data <- x$get()
	        m <- solve(data)
	        x$setinverse(m)
	        m
	}
