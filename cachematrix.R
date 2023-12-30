# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # Initialize a variable to store the cached inverse
  set <- function(y) {
    x <<- y       # Set the matrix
    inv <<- NULL  # Clear the cached inverse
  }
  get <- function() x   # Get matrix from cache matrix object
  set_inverse <- function(inverse) inv <<- inverse  # Set the cached inverse in the cache matrix object
  get_inverse <- function() inv   # Get the cached inverse from the cache matrix object
  list(set = set, get = get,      # Return a list of functions for manipulation 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# Function to compute the inverse of the special "matrix" and use caching
cacheinverse <- function(x, ...) {
  inv <- x$get_inverse()    # Check if the inverse is already cached
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  matrix.to.inverse <- x$get()   # If not cached, calculate the inverse and cache it
  inv <- solve(matrix.to.inverse, ...)
  x$set_inverse(inv)
  inv                          # Return the calculated inverse
}
