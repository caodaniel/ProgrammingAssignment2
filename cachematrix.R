## Write a short comment describing this function

makeCacheMatrix <- function(x =matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function(y) {
    z <- x-y
    if(sum(z)==0 & !is.null(inverse) ) # matched
      return(inverse)
    else return(NULL)
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- x$getInverse(data)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
