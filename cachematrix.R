## Put comments here that give an overall description of what your
## functions do
#### The overall functions below calculate and cache the inverse of a matrix. 


## Write a short comment describing this function
#### makeCacheMatrix function does two things: 
#### 1. create a matrix object x
#### 2. cache the inverse of the matrix x 

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invM <<- inverse
  getinverse <- function() invM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve function computes the inverse of the matrix x
## and compares to the cached value returned by makeCacheMatrix above.
## If the results are the same, the cacheSovle function skips
## calculating the inverse again, and just retrieves
## the inverse from the cache. Otherwise, it 
## calculates the inverse, and stores the result in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinverse(invM)
  invM
}
