## These two functions cache the inverse of a matrix 

## The first function, makeCacheMatrix, creates a special "matrix" object 
## (actually a list) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {           ## set the value of the matrix
               x <<- y
               m <<- NULL
          }
          get <- function() x            ## get the value of the matrix
          setinvert <- function(solve) m <<- solve(x)  ##invert the matrix
          getinvert <- function() m            ## get the inverted matrix
          list(set = set, get = get,
               setinvert = setinvert,
               getinvert = getinvert)
     }

## The second function, cacheSolve, computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
     
     m <- x$getinvert()
     if(!is.null(m)) {
          message("getting cached data (inverted matrix)")
     return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinvert(m)
     m                   ## Return a matrix that is the inverse of 'x'
}