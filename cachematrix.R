## This function caches a Matrix and solves the inverse of the matrix

## This function, `makeCacheMatrix` creates a special "matrix" object, which is
## really a list containing a function to

## 1.  set the matrix in the cache
## 2.  get the matrix from the cache
## 3.  set the matrix inverse in the cache
## 4.  get the matrix inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) m <<- matrix
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## The following function calculates the inverse of the matrix stored
## in the above function. However, it first checks to see if the
## inverse of the matrix has already been calculated. If so, it 
## `get`s the inverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the 
## inverse of the matrix  in the cache via the `setmatrix`function.

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
