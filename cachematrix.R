## Save the value of inverse of a matrix in a chache
## so that it can be used for subsequent calculations

## Make a matrix object that remembers the matrix and 
## stores the matrix inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(minverse) m <<- minverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function takes in the list generated from makeCacheMatrix
## and returns the inverse of the matrix. If the inverse already exists
## it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
