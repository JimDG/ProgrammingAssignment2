# makeCachematrix is a function which takes a matrix M as argument a
# and returns a list of functions: set the matrix; get the matric;
# set the inverse; get the inverse
  
  
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  
# cashSolve computes the inverse of the matrix returned by 
# makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv
  }
