
##function makeCacheMatrix is a function that crate matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

  dim(x)<-c(length(x)/2, length(x)/2)
  
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


## cacheSolve is a function that computes the inverse of squar matrix return by makeCacheMatrix
## if the inverse has already been calculated for current matrix, 
## than cacheSolve return the inverse from a cache

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
