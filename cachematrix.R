## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The below two functions cache the inverse of a matrix.
# Example call
#
# 1. Define matrices
# m2 <- matrix(1:4, nrow = 2, ncol = 2)
# m3 <- matrix(1:4, nrow = 2, ncol = 2)
#
# 2. Store matrices in global memory and register supporting functions
# vm2 <- makeCacheMatrix(m2)
# vm3 <- makeCacheMatrix(m3)
#
# 3. Calculate and export inverse matrices 
# cacheSolve(vm2)
# cacheSolve(vm3)
#
# 3. Read from cache and export inverse matrices 
# cacheSolve(vm2)
# cacheSolve(vm3)


# Store matrix x in cache and register supporting functions 
makeCacheMatrix <- function(x = matrix()) {
  mxi <- NULL # mxi 'MatriX Inversed', 
  
  # write original matrix x into cache
  set <- function(y) {
    x <<- y
    mxi <<- NULL
  }
  
  # get original matrix x from cache
  get <- function() x
  
  # write inversed matrix into cache
  set_mxi <- function(solve) mxi <<- solve
  
  # retrieve inversed matrix from cache
  get_mxi <- function() mxi
  
  # export above data / function call for write into a list
  list(set = set, get = get,
       set_mxi = set_mxi,
       get_mxi = get_mxi)
}



# Calculate / Read from cache and export inverse matrices
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # try to read result data (inversed matrix) from cache
  mxi <- x$get_mxi()
  if(!is.null(mxi)) {
    message("getting cached data")
    return(mxi)
  }
  
  # read original matrix from cache
  data <- x$get()
  
  # do the time-consuming matrix inversion
  mxi <- solve(data, ...)
  
  # store inversed matrix into cache
  x$set_mxi(mxi)
  
  # return inversed matrix
  mxi
}
