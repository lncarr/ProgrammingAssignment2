## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can be used to be
#called to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL 
  }
  get <-function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#this function computes the inverse of the matrix
#the matrix is used after being called using above function
#assume all can be inverted - solve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setInverse(m)
  m
}
