## This entire function is capable of returning the inverse of a matrix

## The first function creates a list which contains the matrix that
##will be inverted, a variable to store the inverse and another to call it

makeCacheMatrix <- function(x=numeric()) {
  m <- NULL
  get <- function() { x }
  setinv <- function(solve) {m <<- solve}
  getinv <- function() m
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calls the inverse of the matrix if it has been already
##calculed and calcules it in case it has not been set previously,
##returning it's value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
