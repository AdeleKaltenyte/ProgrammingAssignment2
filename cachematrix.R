## makeCacheMatrix is a function that performs caching the inverse of a matrix.
## vacheSolve is a function that uses makeCacheMatrix returned matrix to compute the inverse of the matrix.

## makeCacheMatrix & vacheSolve functions are used to create an object that stores a matrix and caches its inverse. 
## makeCacheMatrix creates a special “matrix”, which is a list containing a function to:

    #1. set the value of the matrix

    #2. get the value of the matrix

    #3. set the value of the inverse

    #4. get the value of the inverse

## cacheSolve function computes the inverse of the special “matrix” returned by makeCacheMatrix. 
## cacheSolve retrieves the inverse from the cache if the inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL  
  set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) m <<- solve
    getinverse <- function()m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
