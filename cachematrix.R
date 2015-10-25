
## This function creates a special matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = Matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  return(list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse))
}


## This function computes the inverse of the special matrix created by the function above
## if the inverse has already been calculated, then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
