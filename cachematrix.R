## The overal function takes a value for matrix and stores inverse of that matrix in the cache

## this function takes in a matrix and stores the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(matrix){
    x <<- matrix
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks the cache if the is a data to use for matrix if not it creates one and stores it.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)){
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
