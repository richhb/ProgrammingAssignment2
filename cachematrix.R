## These two functions hold a matrix in memory and provide
## the functionality to also hold the inverse of the matrix
## in memory

## Creates a list with a matrix (as an argument), the inverse
## of the matrix,  and a set of functions that can retrieve
## or change the values of the matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Takes a makeCacheMatrix object as an argument and returns the
## stored inverse matrix or calculates the inverse, stores it in
## the makeCacheMatrix object and returns it

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinv(s)
  s
}
