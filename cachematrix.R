# The first function `makeCacheMatrix` a matrix object that can cache its inverse and the second function `cacheSolve` computes the inverse of the special
# "matrix" returned by `makeCacheMatrix`. The unique thing is that if the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.



# The function makeCacheMatrix looks to creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve gives the inverse of the matrix by getting the inverse already calculated and calculating inverse that hasn't been calculated


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
