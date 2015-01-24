## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}

# test case:
# x <- matrix(c(1,2,3,4), nrow = 2,ncol= 2)
# y = makeCacheMatrix(x)
# cacheSolve(y)
# first run return: cacheSolve(y)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# cacheSolve(y)
# second run return: cacheSolve(y)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5