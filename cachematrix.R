## These functions are used to create a special matrix
## which is able to cache a given inverse and another that is
## used to either retrieves the cached inverse or set the inverse
## of the matrix as the cached inverse

## This function creates a special matrix
## which is able to cache a given inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(cinverse) i <<- cinverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function either retrieves the cached inverse or sets the inverse
## of the matrix as the cached inverse
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
