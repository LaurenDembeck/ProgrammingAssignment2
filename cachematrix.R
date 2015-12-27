#The makeCacheMatrix function will generate a list of functions(set, get, setinverse, and getinverse), which can be assigned to a variable
#It aslo sets a matrix, x, to exist in the global environment
makeCacheMatrix <- function(x) {
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

#The cacheSolve function will take a variable, x (the variable that you assigned makeCacheMatrix to)
#It will check if the inverse for the matrix now contained in makeCacheMatrix is null or not
#If null, it solves the inverse of the matrix
cacheSolve <- function(x) {
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