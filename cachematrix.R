## A pair of functions that cache the inverse of a matrix

## function to create a special "matrix" object that can
## cache its inverse

## supply a matrix
makeCacheMatrix <- function(x = matrix()) {

  ## check the matrix supplied is invertible
  if (det(x) == 0) {        
    message("Error: this matrix cannot be inverted")
  } 
  ##the matrix supplied is invertible, make cache matrix
  else {                
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


## function to calculate the inverse of the matrix, unless the 
## matrix was already inverted, in which case it gets the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## check if it exists in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise invert it here
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
