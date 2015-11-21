## Functions that allow to cache the results of the matrix inverse operation
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) minv <<- inverse
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function compute the inverse of the special matrix returned by makeCacheMatrix function
## If the inverse has already been calculated and the matrix is not changed, then cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    ## Return cached data 
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
