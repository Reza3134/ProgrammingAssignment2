## Put comments here that give an overall description of what your

## these functions can set a value for a matrix(makeCacheMatrix) and by using cacheSolve,
##inverse of the matrix will be calculated and saved, in order to prevent repeating the same calculation



## creates a global object x, and set it with the given matrix, have some functions to set and get the data and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if the inverse of a matrix is already calculated, it returns, and if not, it will calculate and save it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
