## The following functions calculate the inverse of a matrix. It returns from cache
## if the inverse is pre calculated

## Returns a list of getters and setters for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns an inverse of a matirx from cache otherise calculates
## it and stores in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    return(i)
  }
  y <- x$get()
  i <- solve(y)
  x$setinverse(i)
  return(i)
  
}
