
## This function defines the matrix and its inverse and how the data calculated is to be cached(stored) in a list.

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {invMat <<- inverse}
  getinv <- function() {invMat}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function performs 2 functions. First, it checks whether inverse of a matrix has been calculated and stored in cache.
## The second function it performs is to calculate the inverse of a matrix using 'solve' function.
cacheSolve <- function(x, ...) {
        
  invMat <- x$getinv()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinv(invMat)
  { invMat }
}
