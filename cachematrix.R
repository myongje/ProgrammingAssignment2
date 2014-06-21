## Put comments here that give an overall description of what your
## functions do
##
## This code supports matrix object that can be used to calculate
## and cache inverse for faster future calculations.
##
## Usage Example:
##
## m<- makeCacheMatrix( )
## m$set( matrix( c(0, 0.5, 2, 0 ), 2, 2))
## m$get()
## cacheSolve( m )
## cacheSolve( m )    ## cached solution returned
##
##
## Write a short comment describing this function
##
## This function create the matrix object with ability to store cached
## solution to cached inverse i.e. solve()
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
##
## This function calculates the inverse and caches on the first call and
## returns cached solution subsequently until matrix is set()
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

