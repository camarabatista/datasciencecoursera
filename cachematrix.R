## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# Set the value of the matrix
# Get the value of the matrix
# Set the value of inverse of the matrix
# Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This is the function that gets the inverse of the matrix. 
## Firstly, it checks if inverse has already been computated. 
## If yes, it gets the result. If no, it makes the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
