## Put comments here that give an overall description of what your
## functions do

## This function sets the value of a matrix object and caches its inverse

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix above if the value has not already been calculated, otherwise it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
