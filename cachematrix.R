##################   FILE: cachematrix.R    ###############################
#####  Author: Jaya Kudaravalli
#####  Date:   11/19/2014
#####  Description:
#####  This file contains two functions makeCacheMatrix() and cacheSolve()
#####  Please see function headers for description
###########################################################################




##################  FUNCTION : makeCacheMatrix() ##########################
## This function creates a list with the functions:
## set(), get(), setinverse(), getinverse() 
## For this function to work correctly the input data 
## must form a inversible matrix
## 
## set() - set the value of matrix 
## get() - get the value of matrix 
## setinverse() -  set inverse matrix  
## getinverse() -  get inverse matrix 
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  
    im <- NULL
    set <- function(y) {
        ##message("Setting matrix")
	    x <<- y
	    im <<- NULL
	}

	get <- function() {
	    ##message(" Getting matrix")
		x
	}

    setinverse <- function(invmatrix) {
 	    ##message("Setting inverse matrix")
	    im <<- invmatrix
    }

    getinverse <- function() {
        ##message("Getting Inverse matrix")
        im
    }
								  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##################  FUNCTION : cacheSolve() ##################################
## This function creates inverse matrix for a special matrix created by
## previous function makeCacheMatrix()
## If an inverse matrix was never created, then it will compute inverse matrix.
## If inverse matrix were created earlier and the original matrix did not change
## then it returns "inverse matrix" computed earlier and stored in cache
##############################################################################

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached Matrix")
        return(im)
    }

    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}




