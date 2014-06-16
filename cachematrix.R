## makeCacheMatrix is a function which stores a given matrix
## and returns a list with 4 functions
## to get/set a matrix
## and to get/set inverse


makeCacheMatrix <- function(sourceMatrix = matrix()) {
  cachedInverse <- NULL
  cachedMatrix <- sourceMatrix
  set <- function(sourceMatrix) {
    cachedMatrix <<- sourceMatrix
    cachedInverse <<- NULL
  }
  get <- function() cachedMatrix
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a 'special' matrix returned by makeCacheMatrix
## and checks if there's cached inverse, it returns it
## otherwiser calculates it, stores in cache and returns the result

cacheSolve <- function(specialMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- specialMatrix$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached inverse...")
    return(inverse)
  }
  message ("Calculating and caching inverse...")
  data <- specialMatrix$get()
  inverse <- solve(data, ...)
  specialMatrix$setInverse(inverse)
  inverse
}

# Usage example:
# m <- matrix(c(1,2,3,0,4,5,1,0,6), nrow=3, ncol=3)
# sm <- makeCacheMatrix(m)
# i <- cacheSolve(sm)
# i