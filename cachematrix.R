## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" which let's us use a cached
## value of the matrix inverse (if available)
makeCacheMatrix <- function(x = matrix()) {
   invx <- NULL
   set <- function(y) {
      x <<- y
      invx <<- NULL
   }
   get <- function() x
   setinverse <- function(inv) invx <<- inv
   getinverse <- function() invx
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##  returns cached value of the inverse if available, else computes the
## inverse, caches the value and then returns the cached value
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   invx <- x$getinverse()
   if(!is.null(invx)) {
      message("returning cached value of the inverse")
      return(invx)
   }
   mat <- x$get()
   invx <- solve(mat)
   x$setinverse(invx)
   invx
}
