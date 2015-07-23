# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function makeCacheMatrix
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	#make sure x is a matrix
	if(!is.matrix(x)) stop("x must be a matrix")
    
	xInv <- NULL
	
	#set the value of the matrix
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
	
	#get the value of the matrix
    get <- function() x
	
	#set the value of inverse of the matrix
    setInv <- function(inverse) xInv <<- inverse
    
	#get the value of inverse of the matrix
	getInv <- function() xInv
		
	list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function cacheSolve
#get Inverse of x from the environment and check if cached
#if it is cached, return. Else cached

cacheSolve <- function(x, ...) {


    xInv <- x$getInv()
	
    if(!is.null(xInv)) {
        print("Cached Data: Inverse of the Matrix exists")
        
		return(xInv)
    }else{
		print("First Time Cached")
		
		xMatrix <- x$get()
		xInv <- solve(xMatrix)
		x$setInv(xInv)
		
		return(xInv)
	}
}