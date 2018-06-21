## Writing functions to make a cache matrix object, and then solve for its inverse

## First function takes a matrix, then creates a matrix object/list that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set.inv <- function(solve) m <<- solve
      get.inv <- function() m
      list(set = set, get = get,
           set.inv = set.inv,
           get.inv = get.inv)

}


## Second function computes inverse of matrix object returned by makeCacheMatrix.
## If inverse already calculated and matrix hasn't changed, function retrieves inverse
## from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      # inv <- x$solve()
      m <- x$get.inv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set.inv(m)
      m
}
