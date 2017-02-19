## Put comments here that give an overall description of what your
## functions do

#Creates a special "matrix" object 
#that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_imatrix <- function(solve) m <<- solve
  get_imatrix <- function() m
  list(set = set, get = get,
       set_imatrix = set_imatrix,
       get_imatrix = get_imatrix)
}

#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  m <- x$get_imatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_imatrix(m)
  m
}
