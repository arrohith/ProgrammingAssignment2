## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Both functions help in caching the iversing the contents of the matrix
## By creating an object called "matrix", we are caching it's reverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  set_inverse <- function(inv) { inverse <<- inv }
  get_inverse <- function() { inverse }
  list(set = set, get = get, set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()  
  if(!is.null(inv)) {
    message("getting cached data.........")
    return(inv)
  }
  my_matrix <- x$get()
  inv <- solve(my_matrix, ...)
  x$set_inverse(inv)
  inv
}
