## Put comments here that give an overall description of what your
## functions do

# The first function, makeVector creates a special "vector", which is really a list 
# containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of matrix
# 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with the makeCacheMatrix-function. 
# First, this function checks if the inverse was calculated. If yes [!(is.null(inv))], it sends a 
# message saying "getting cached data". Otherwise, it calculates and save the result.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
