## Second programming assignment in R Programming, this functions will cache potentially time-consuming computations
## as computing over a large vector

## This function creates a special "matrix" object that can cache 
##its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get function
  get <- function() x
  ##setting the returning matrix
  setinverse <- function(solve) m <<- solve
  ##Getting the actual matrix, if theres any
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


##This function computes the inverse of the special "matrix"
##returned by the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  ## Calling getinverse function
  m <- x$getinverse()
  ##Testing if the resultin "m" it is not NULL, if so return cached m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if its NULL get the data
  data <- x$get()
  ##inverse de matrix
  m <- solve(data)
  ##setting the inverse matrix to m, calling setinverse function
  x$setinverse(m)
  m
}