## makeCacheMatrix function creates a list of 4 functions: 1 to set values of a matrix ; 2 to get values of the matrix ; 
## 3 to set inverse of matrix by passing the calculated inverse value as argument and 4, to retrieve the inverse of the matrix 
## either from cache (if available) or to calculate and retrieve 

## This function creates the list of functions as described above

makeCacheMatrix <- function(x = matrix()) {
matxinv <- NULL
  set <- function(y) {
    x <<- y
    matxinv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matxinv <<- inverse
  getInverse <- function() matxinv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns a matrix that is the inverse of x 

cacheSolve <- function(x, ...) {
       matxinv <- x$getInverse()
  if (!is.null(matxinv)) {
    message("Retrieving Inverse of Matrix from Cache")
    return(matxinv)
  }
  mtx <- x$get()
  matxinv <- solve(mtx, ...)
  x$setInverse(matxinv)
  matxinv
        
}
