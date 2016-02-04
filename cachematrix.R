## Caching the Inverse of a Matrix
## Assigment: Programming Assignment 2: Lexical Scoping 
## We are going to introduce the <<- operator and  cache the value.
## Caching will help us in cases where there is a costly   computation like Matrix 
## inversion. Here inverse of matrix is cached rather than computing it repeatedly.

## makeCacheMatrix is the function which creates the matrix object that can cache
## its reverse.

makeCacheMatrix <- function(x = matrix()) {
      rev <- NULL
      set <- function(y) {
        x <<- y
        rev <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) rev <<- inverse
      getInverse <- function() rev
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of matrix returned by the function 
## makeCacheMatrix. If the inverse is already calculated, then this function should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  rev <-x$getInverse()
  if(!is.null(rev)){
    message("Getting cached data")
    return(rev)
  }
  mat <- x$get()
  rev <- solve(mat, ...)
  x$setInverse(rev)
  rev
}
