## A pair of functions for inverting a matrix using cacheing based on R lexical scoping rules.
## Cousera programming assignment week 3.

## Return a list containing cache and mutator, accessor functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## Return a matrix that is the inverse of 'x', where x is the list created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  mtrix <- x$getinverse()
  if(!is.null(mtrix)) {
    message("getting cached data")
    return(mtrix)
  }
  mtrix <- x$get()
  
  ## Sanity checks
  if (nrow(mtrix) != ncol(mtrix)) {
    message("non-square matrices do not have inverses!")
    return(NULL)
  }
  
  if (nrow(mtrix) < 20 & det(mtrix) == 0)  {  ## 20: This sanity check may be too expensive for large matrices.
      message("matrices with determinate 0 do not have inverses!")
      return(NULL)
  }
  
  m <- solve(mtrix, ...)  ## solve inverts the matrix
  x$setinverse(m)
  m
}
