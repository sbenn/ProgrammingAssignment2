## Matrix inversion is usually a costly computation 
## If repeated access to an inverted matrix is required
## the functions makeCacheMatrix and cacheSolve work in pair
## to get the inverse of a matrix with automatic
## retrieval from a cache on subsequent calls

## Usage:
## sample_matrix <- matrix( rnorm(4*4,mean=0,sd=1), 4, 4)
## cache_matrix <- makeCacheMatrix(sample_matrix)
## cacheSolve(cache_matrix)


## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(regMatrix = matrix()) {
  ## Validate input parameter
  if (is.null(regMatrix) || any(is.na(regMatrix))) {
    stop("NULL or NA not supported")
  } else if (!is.matrix(regMatrix)) {
    stop("only matrices can be used")
  }
  else if (nrow(regMatrix) != ncol(regMatrix)) {
    stop("only square matrices can be used")
  }
  
  invMatrix <- NULL
  set <- function(y) {
    regMatrix <<- y
    invMatrix <<- NULL
  }
  get <- function() regMatrix
  setsolve <- function(solve) invMatrix <<- solve
  getsolve <- function() invMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve retrieves the
## inverse from the cache.
cacheSolve <- function(x, ...) {
  invMatrix <- x$getsolve()
  if(!is.null(invMatrix)) {
    message("getting inverted matrix from cache")
    return(invMatrix)
  }
  regMatrix <- x$get()
  invMatrix <- solve(regMatrix, ...)
  x$setsolve(invMatrix)
  invMatrix
}