## Function pair to create a matrix and cache its inverse


## Create a closure containing a function and associated environment 
## ...in which to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 			##This is the local version of the inverse. 
						##   To make sure it is NULL with every new call
        setm <- function(y) {		##function to: 
                ma <<- y		##   set matrix ma to y ( set it in the parent envt - makeMatrix())
	          inv <<- NULL		##   set the cached inverse to NULL (parent envt) 
        }
        getm <- function() ma		##function to return the matrix ma
        setinv <- function(inverse) inv <<- inverse 
						##function to set the cached inverse (inv) to the value passed to the function
        getinv <- function() inv
						##function to return the value of cached inverse
        list(setm = setm, getm = getm,
             setinv = setinv, 
             getinv = getinv)
						##return a list of all 4 functions
}



## Access the closure created in make CacheMatrix(), 
## check if the inverse is cached and return, else calculate inverse and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() 			##call the function getinv() to get the cached inverse 
	  if(!is.null(inv)) { 			##only do this if inv exists (if inv existed inside the 
							##environment associated with closure x
                message("getting cached inverse")
                return(inv)			##return, we have used the cached inverse, no need to calculate
        }
        data <- x$getm()			##call the function getm() - inside the closure x to get the matrix
        inv <- solve(data)			##calculate the inverse of the matrix
        x$setinv(inv)				##set the inv in the closure  i.e cache it for next time
        inv						##return the inverse
}
