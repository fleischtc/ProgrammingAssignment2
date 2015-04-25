## These set of functions are designed to cache time-consuming computations - 
## specifically, these will generate and cache the inverse of a matrix.
## Input:  a square matrix that is invertible

## The first function will create a special object that stores a matrix(m) and
## its inverse (inv) 


makeCacheMatrix <- function(m = matrix()) {

  inv <- NULL               ##iniate empty matrix for inv
  set <- function(y) {      ## set up cache for matrix(m) and it's inverse(inv) 
      m <<- y
      inv <<- NULL
  }
  get <- function() m
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, 
      get = get,
      setinv = setinv,
      getinv = getinv) 
}


## This 2nd function will retrieve the inverse of a matrix from a cache, if it 
## exists.  Otherwise, it will generate the inverse of the matrix and cache it.

cacheSolve <- function(m, ...) {

  ## Return a matrix that is the inverse of 'm'
 
  inv <- m$getinv()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinv(inv)
  inv
}


