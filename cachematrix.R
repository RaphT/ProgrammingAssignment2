## The functions here enable the caching of the inverse of a given square matrix
## in order to save time in case of multiple calls

## This function creates a list containing the square matrix to be inverted, 
## plus set and get functions for (1) the matrix itself and (2) the calculation
## of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #Create the inverse
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this functions takes as input a list created by makeCacheMatrix and 
## outputs the inverse of the matrix contained in the list. If the inverse has
## already been calculated for this matrix is it simply retrieved from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}


# These are few lines to test my functions ####
#Create a square matrix
nlines = 1000
myMatrix <- matrix(round(rnorm(nlines*nlines,10,10)),nlines,nlines)
#"Cache it"
myCacheMatrix <- makeCacheMatrix(myMatrix)
#Cache inverse it for the first time and time it
system.time(myInvCacheMatrix <- cacheSolve(myCacheMatrix))
#Do it again to figure the speedup (huge gain)
system.time(myInvCacheMatrix <- cacheSolve(myCacheMatrix))
#Check if myMatrix %*% Inverse is identical to the identity matrix
identical(diag(nlines), round(myMatrix%*%myInvCacheMatrix),5)

