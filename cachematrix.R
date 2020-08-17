## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is meant to store both the matrix and its inverse 
## cacheSolve first searches makeCacheMatrix for a solution, and if it doesn't, 
  ## calculates the inverse

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input stores the matrix in get, and the inverse in getinv

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set<- function(y){
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setinv <-function(solve) c <<-solve
  getinv <- function() c
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve retrieves an inverse if the matrix has been cached, but otherwise goes about 
  ## solving the matrix (stored as c, here). Input is actually makeCacheMatrix(x), not matrix x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  c <- x$getinv()
  if(!is.null(c)){
    message("getting cached data")
    return(c)
  }
  data<- x$get()
  c <- solve(data, ...)
  x$setinv(c)
  c
}
