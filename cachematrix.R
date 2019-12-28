## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set = function(y) {
    x <<- y
    inverse <<- NULL
  }
  get = function() x
  setinv = function(inverse) inverse <<- inverse 
  getinv = function() inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inverse = x$getinv()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat.data = x$get()
  inverse = solve(mat.data, ...)
  x$setinv(inverse)
  
  return(inverse)
}