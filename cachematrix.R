
##makeCacheMatrix takes in a square invertible matrix as input and creates a list containing
##functions to set and get the value of the matrix. Also the list contains set and get
##functions for the inverse of the inour matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##set input matrix and inverse of the matrix to cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##function to get value of matrix
  get <- function() x
  ##function to set inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  ##function to get inverse of matrix
  getinverse <- function() inv
  ## return list of above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve returns the inverse of the matrix created by the makeCacheMatrix
##function. If the inverse is already calculated it gets it from the cache otherwise
##it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ##check if inverse is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x'
    return(inv)
  }
  ##get the data of the matrix created by makeCacheMatrix function
  data <- x$get()
  ##calculate the inverse
  inv <- solve(data, ...)
  ##cache the inverse
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}


