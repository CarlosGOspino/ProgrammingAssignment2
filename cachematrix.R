## These functions cache the inverse of a matrix to save on processing time

## makeCacheMatrix creates a list to:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse of the matrix
## 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  n<- NULL
  set<- function(y) {
    x<<-y
    n<-NULL
  }
  get<-function() x
  setinv<-function(inv) n <<- inv
  getinv<-function() n
list((set = set, get = get,
      setinv = setminv,
      getinv = getinv))
}


## This function calculates the inverse of a matrix in case the matrix has not changed
## if the matrix changes it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n<- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
