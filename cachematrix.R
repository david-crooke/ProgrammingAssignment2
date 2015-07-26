## Assignment2 .. Caching the Inverse of a Matrix
## author: dc   date: 20150726
## *** NOTE: Check for non-invertable matrix input, ie determinant of input matrix == 0,  is commented out.


makeCacheMatrix <- function(x = matrix()) {
  #
  # makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  #
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # if (det(x)==0) {stop("makeCacheMatrix: The determinant of input matrix 'x' is 0")}
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


cacheSolve <- function(x=matrix(), ...) {
  #
  # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse matrix from the system cache
  #
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if (det(x)==0) {stop("cacheSolve: The determinant of input matrix 'x' is 0")}
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

