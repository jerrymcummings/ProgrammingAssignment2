## This module contains two function that can potentially
## speed up the caller's use of matrix inversion if they
## are inverting the same matrix more than once.
##


## Create a cached_matrix object (actually, a well-prepared
## list of helper functions) given a matrix for which
## the caller will want the inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  # init the solution to null so that we
  # know if we need to compute it or not
  the.inverse <- NULL
  
  # store a new matrix and reset any cached
  # inverse to NULL
  set <- function(y) {
    x <<- y
    the.inverse <<- NULL
  }
  
  # helper to return the source matrix
  get <- function() x
  
  # helper to cache the inverse (as computed by the caller)
  set.inverse <- function(inv) the.inverse <<- inv
  
  # return the cached inverse (which could be null if not
  # yet computed)
  get.inverse <- function() the.inverse
  
  # return the list that represents this cached matrix object
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Primary function to compute and cache the inverse
## of a given matrix.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  # see if this already has a cached inverse
  inv <- x$get.inverse()
  
  # if it does, we can return right now
  if (!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  
  # if it doesn't have a cached inverse, we'll
  # need to compute it.
  # 
  # - get the data
  mtrx <- x$get()
  
  # - compute the inverse
  inv <- solve(mtrx, ...)
  
  # - cache the inverse
  x$set.inverse(inv)
  
  # - done. return the inverse
  inv
}

## test code
if (TRUE) {
  test.matrix <- rbind(c(1, -1/4), c(-1/4, 1)) 
  test.matrix.cache <- makeCacheMatrix(test.matrix)
  inv <- cacheSolve(test.matrix.cache)
  print(test.matrix)
  print(inv)
  
  inv <- cacheSolve(test.matrix.cache)
  print(inv)  
  
  inv <- cacheSolve(test.matrix.cache)
  print(inv)  
  
  message("Expect to see:")
  message("          [,1]      [,2]")
  message("[1,] 1.0666667 0.2666667")
  message("[2,] 0.2666667 1.0666667")
}
