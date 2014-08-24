## makeCacheMatrix function creates a special "matrix" 
## and cacheSolve  calculates its inverse or returns the cached one. 




## Creates special "matrix" which is a list 
## containing a function to each of the following
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##setup the value of the inverse matrix function 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  ## create oblect
  message("setting   data")
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## It's there so return the value of the inverse matrix
    return(m)
  }
  ## we are here so its not there.
  ## Create inverse of input matrix and
  ## cache the value of the inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
