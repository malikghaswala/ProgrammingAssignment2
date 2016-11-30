## Caching the Inverse of a Matrix by MG
## Matrix inversion is usually a costly computation 
## and there maybe some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## The below function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The below function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting inverse cached data")
    return(invr)
  }
  matr <- x$get()
  invr <- solve(matr, ...)
  x$setinverse(invr)
  invr
}

## in addition please find below the results that the above functions are working
> mg <- makeCacheMatrix(matrix(1:4,2,2))
> mg$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> mg$getinverse()
NULL
> cacheSolve(mg)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(mg)
getting inverse cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
          
          
