####
###makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

#Passing in a matrix() argument caused a "must be square error', so makeCacheMatrix was rewritten to handle that.
#The main funtion takes nrows and ncol arguments along with vector.


makeCacheMatrix <- function(number,irows,icols) {
  m <- NULL
  x <- matrix(number, irows, icols)
  set <- function(ynumber, yrows, ycols) {
    x <<- y <- matrix(ynumber, yrows, ycols) #allow for setting/assigning matrix values globally
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("Inverse cached value retrieved")
    return(m)
  }else {
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
  }
}

#Test case1 with square matrix:
inverseResult <- makeCacheMatrix(c(3,2,0,0,0,1,2,-2,1),3,3)
inverseResult$get()
cacheSolve(inverseResult)

#Test case2 sets matrix values and runs function:
#inverseResult$set(2,1,1)
#inversResult$get()
#cacheSolve(inverseResult)

