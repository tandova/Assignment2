## Put comments here that give an overall description of what your
## functions do

## This function returns a list containing a reference to each function inside it: set, get, setinverse and getinverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- matrix()
    set <- function(y) {
      x <<- y                                ##setting the initial matrix
      inverse <<- NULL
    }
    get <- function() x                      ##getting the initial matrix
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of a matrix 'x' the first time it's called
##Every other time, it calls the inverse from cache and returns it, instead of calculating it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <-x$getinverse()
  if(!is.na(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}