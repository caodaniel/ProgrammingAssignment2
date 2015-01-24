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
    if(all(x==y) & !is.null(inverse) ) # matched
      return(inverse)
    else return(NULL)
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
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
