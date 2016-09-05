## Put comments here that give an overall description of what your
## functions do

## Create special matrix object

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y){
	x <<- y
	inv <<- NULL
}
get = function()x
setinv = function(inverse)inv <<- inverse
getinv = function()inv
list(set=set,get=get, setinv=setinv, getinv=getinv)
}


## calculate the inverse of the matrix, if the inverse calculated, cache and return, if not calculated, calculate.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  

  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
#solve inverse if inv is NULL
  mat.data = x$get()
  inv = solve(mat.data, ...)
  

  x$setinv(inv)
  
  return(inv)
}
