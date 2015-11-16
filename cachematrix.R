## The function makeCacheMatrix creates an environment for setting, and getting
## a square matrix and its inverse assuming that the matrix is invertible.
## The cacheSolve function makes use of the makeCacheMatrix function, which
## should have been called prior to invoking the cacheSolve function
## so that it can set the inverse of the matrix created with makeCacheMatrix()
## or retrieve it if it has already been set.

## makeCacheMatrix creates individual functions that can be used to 
## either set or get a matrix specified through the x argument
## and set or get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- matrix(0,0,0)
  set <- function(y) {
    x <<- y
    i <<- matrix(0,0,0)
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function first checks to see if the inverse of a
## matrix previously created through the makeCacheMatrix function 
## already has had its inverse calculated. iF so, it gets the matrix
## inverse that was previously calculated and stored. Otherwise, it calculates
## the matrix inverse and it stores it through the setinverse() function of
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(length(i)!=0) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
