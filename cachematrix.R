## Put comments here that give an overall description of what your
## functions do

## This function returns a list that sets the matrix and the inverse 
## Note that prior to running this function, the matrix must already be defined as a square matrix

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x  
  setinv <- function(solve) n <<- solve(x)  ##Calculates the inverse of the matrix
  getinv <- function() n   ## Provides quick access to inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  ## Returns a list setting and getting values and inverses
}




## This function tests to see if the inverse has already been calculated for the matrix and returns
## a cached value if so
## If not, it calculates and returns the inverse

cacheSolve <- function(x, ...) {
  
  
  n <- x$getinv()
    if(!is.null(n)) {   ##tests to see if the inverse has already been calculated and returns cached value
        message("getting cached data")
        return(n)
  }
  data <- x$get()    ## If the inverse hasnt already been calculated, this function calculates the inverse
  n <- solve(data, ...)
  x$setinv(n) 
  n
  
}
