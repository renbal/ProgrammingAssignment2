## Functions makeCacheMatrix and cacheSolve aim at cutting down the cost of matrix inversion
## calculations by caching the inverse of a matrix as opposed to computing it
## repeatedly. To achieve this a special "matrix" object is created which is able
## cache its own inverse.



## makeCacheMatrix creates a special "matrix" which is a list containing
##functions to set the values of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse to NULL
  inv <- NULL
  
  ## function to set matrix data
  set <- function(y) {
    x <<- y
    ## set inverse value to NULL each time the matrix changes since
    ##inverse would have to be recalculated
    inv <<- NULL
  }
  
  ##function to return matrix
  get <- function() x
  
  ## function to set and cache the value of the matrix inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ##function to return the cached value of the matrix inverse
  getinverse <- function() inv
  
  ##return list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve calculates the inverse of a special "matrix" (as created in makeCacheMatrix)
## It would, however, first check to see if the inverse has already been calculated. 
## If so, and the matrix has not changed, it would return the inverse from the cache and 
## skip the computation. Otherwise, it would calculate the inverse and accordingly set
## the the value of the inverse in the cache of the special "matrix" object via the 
## setinverse function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## if inv is not NULL then cached value is returned
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if inv was NULL then inverse must be recalculated and cached via setinverse function
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  ##return the inverse of X
  inv
}
