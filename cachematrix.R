## This Program creates two functions.  
## The first function creates and caches a Matrix.
## The second fucntion computes the inverse of Matrix if Matrix has
## has not been cached.  If cached, it retrieves the inverse from the cache.

## This funtion creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Matrixinv <- NULL                                                                       ## Initialize inverse as null
  set <- function(y){
    x <<- y                                                                               ## Value of matrix
    Matrixinv <<- NULL                                                                    ## Reset matrix to null if new matrix
  }
  get <- function() x                                                                     ## Define get function.  Return value of matrix.
  setInverse <- function(inverse) Matrixinv <<- inverse                                   ## Assigns value of Matrixinv  
  getInverse <- function() Matrixinv                                                      ## Gets value of Matrixinv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)            ## Create a list, so can refer to arguments with $ sign.
}


## Checks if special matrix created above has been cached.  If yes, it retrieves it.  If not, it computes inverse and caches it.

cacheSolve <- function(x, ...) {
  Matrixinv <- x$getInverse()                        
  if(!is.null(Matrixinv)) {                          ## Checks to see if inverse is cached.  If yes, return the caches inverse.
    message("getting cached data")
    return(Matrixinv)
  }
  mat <- x$get()                                     ## Gets the matrix
  Matrixinv <- solve(mat, ...)                       ## Solve for inverse using solve function
  x$setInverse(Matrixinv)                            ## Cache the inverse
  Matrixinv
}
