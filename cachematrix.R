##  The cacheSolve function will produce the inverse of a matrix.  If the matrix data has not changed (i.e., 
##  the user has not run makeCacheMatrix again), then the cacheSolve function will used a cached
##  version of the result instead of calculating the result on the fly using solve.
##
##  Example Usage: im<-makeCacheMatrix(matrix(c(rnorm(16, 1, 10)), nrow=4, ncol =4)) ##create a matrix and set the base property of makeCacheMatrix 
##  cacheSolve(im) ##produce the inverse of the matrix (assuming it is invertible).
##
## Get and set cached variables

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Produce the Inverse of the given matrix via the cache if available using makeCacheMatrix$get 
## or by using solve() and makeCacheMatrix$setInverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
