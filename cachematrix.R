## These two functions in combination are to find calculate, store, and return 
## inverse of any given matrix (that is invertible)

## The function "makeCacheMatrix" contains a set of subfunctions that are to 1. reset to original value
## 2. get matrix data and store 3. set a stored matrix's invertible one 4. return stored invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(myinverse) m <<- myinverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" is to return stored invertible matrix, if that matrix is already processed
## at least once before. If not, this function will calculate invertible matrix, and store it into 
## the list from previous function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
