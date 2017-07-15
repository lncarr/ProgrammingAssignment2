## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can be used to be
#called to cache its inverse 
##created the same as the example - make sure to NULL and set it and use the functions set/get

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
#solved about the same way as other one 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)     ##had inverse instead of solve but solve needs to be used to do the inverse in R 
  x$setInverse(m)
  m
}
