# Functions bellow allow to compute the inverse of a matrix
# Inverting a matrix is a costly operation, so it's beneficial to cache the result
# We are assuming that a matrix is invertible


# This function creates a list object that enables us set and get a value of a matrix
# and set and get a value of inverse of a matrix

makeCacheMatrix <- function(matrix = matrix()) {
  inverse_matrix <- NULL
  set <- function(x) {
    matrix <<- x
    inverse_matrix <<- NULL
  }
  get <- function() matrix
  set_inverse <- function(inverse) inverse_matrix <<- inverse
  get_inverse <- function() inverse_matrix
  return(list(set = set, get = get, set_inverse=set_inverse, get_inverse=get_inverse))
}

# This function returns the inverse of a matrix. First of all, the function checks
# is the inverse has been computed before (and the matrix has not changed), then
# the function gets the previous result and skips the computation. Otherwise, the
# function computes the inverse and writes the value to cache

cacheSolve <- function(matrix, ...) {
  inverse_matrix <- matrix$get_inverse()
  if(!is.null(inverse_matrix)) {
    message("Getting cached data...")
    return(inverse_matrix)
  }
  data <- matrix$get()
  inverse_matrix <- solve(data, ...)
  matrix$set_inverse(inverse_matrix)
  return(inverse_matrix)
}
