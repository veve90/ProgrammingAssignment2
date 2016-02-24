## Matrix inversion is usually a costly computation

## Therefore sometimes we may take benefit of caching 
## the inverse of the matrix rather than compute it each time.
#
# Example of run:
# x = rbind(c(1, -1/4, 1/3), c(-1/4, 1, 1), c(1, 1/2, 1/2))
# m <- makeCacheMatrix(x)
# cacheSolve(m)
#           [,1]       [,2]       [,3]
# [1,]  0.000000 -0.4444444  0.8888889
# [2,] -1.714286 -0.2539683  1.6507937
# [3,]  1.714286  1.1428571 -1.42857
# 
# cacheSolve(m)
# getting cached data
#           [,1]       [,2]       [,3]
# [1,]  0.000000 -0.4444444  0.8888889
# [2,] -1.714286 -0.2539683  1.6507937
# [3,]  1.714286  1.1428571 -1.4285714
# 



## The function "makeCacheMatrix" creates a list containing 4 function  that enable us to:
# 1.set the value of the matrix ==> set
# 2.get the value of the matrix ==> get
# 3.set the value of the inverse of the matrix ==> setinverse
# 4.get the value of the inverse of the matrix ==> getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function get us the inverse of a "special matrix" 
## that we got from calling "makeCacheMatrix" 
# If the inverse has already been calculated ,
# than the answer will be given from the cache.  
# This function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
