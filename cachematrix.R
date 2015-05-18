## Functions to cache the inverse of a matrix


## makeCacheMatrix: to create a special "matrix" containing a list of functions to
# -- set the value of the matrix
# -- get the value of the matrix
# -- set the inverse of the matrix
# -- get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL # to store the inverse of x
  
  # the set function
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  
  # the get function
  get <- function() {
    x
  }
  
  # the setinv function to set the inverse
  setinv <- function(someinv) {
    x_inv <<- someinv
  }
  
  # the getinv function to get the inverse
  getinv <- function() {
    x_inv
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: to calculate the inverse of the matrix
# -- if the inverse is cached, the function retrieves it from the cache
# -- otherwise, the function calculate the inverse

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv() # try to get the inverse from cache
  if (!is.null(x_inv)) {
    # the inverse is cached, just return it
    message("getting cached data...")
    return(x_inv)
  }
  # at this point, the inverse is not cached
  # just solve for it, and cache
  mat <- x$get()
  message("solving for inverse...")
  mat_inv <- solve(mat)
  x$setinv(mat_inv)
  mat_inv
}
