## 23-04-2017; t. van Dam

## this R function will take as input a matrix and cache its inverse
## code is based on the code makeVector and cacheMean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invSolve) inv <<- invSolve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}  


## this function caches the inverse matrix
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatritx
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}