## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a cache for a given
## matrix.  It is assumed that a given matrix is a invertible square matrix. 
##

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y){
    if((class(y) == "matrix") & (dim(y)[1] == dim(y)[2])){
      mtx <<- NULL
      x <<- y
    }else{
      print("Not a proper input.  Provide an invertible square matrix")
      return()
    }
  }
  get <- function() x
  setInv <- function(inv) mtx <<- inv
  getInv <- function() mtx
  invisible(list(set = set, get = get, setInv = setInv,getInv = getInv))
}


## Write a short comment describing this function
## cacheSolve is a function that checks if there is a cache for the inverse
## of the given cachable matrix (not an atomic matrix) and retrieves the inverse
## from cache, if one exits. Else, it calculates the inverse by calling solve function
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mtx <- x$getInv()
	if(!is.null(mtx))
	{
		message("getting cached data")
		return(mtx)
	}
	data <- x$get()
	mtx <- solve(data,...)
	x$setInv(mtx)
	mtx
}
