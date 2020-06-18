## A pair of functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
      	    x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solv) m <<- solv
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function creates a special matrix object that can cache its inverse.
cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      matr <- x$get()
      m <- solve(matr)
      x$setsolve(m)
      m
}
