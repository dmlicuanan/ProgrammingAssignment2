## The makeCacheMatrix and cacheSolve functions help speed up the 
## process of returning the inverse of an invertible matrix by replacing the
## (time-consuming) computation step with the printing of the cached inverse if 
## the inverse has been previously computed.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)

}

## The cacheSolve function computes for the inverse of the special
## "matrix" provided by the makeCacheMatrix function, if the 
## inverse of the said matrix has not been previously computed.
## Otherwise, it will return the inverse cached by the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse of matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
