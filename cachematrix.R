## Put comments here that give an overall description of what your
## functions do
## This pair of functions  caches  the inverse of a invertible matrix


## This function returns a "special matrix" or a list containing  the 
##  functions for  storing, caching and retrieving  the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y 
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inversa) Inv <<- Inversa
  getInv <- function() Inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns the inverse of a invertible matrix. This funtion 
## evaluates if the matrix has changed and if its inverse has already been 
## calculated. 

cacheSolve <- function(x, ...) {
  ## "x" indicating  a "special matrix" or list returned by makeCacheMatrix()
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)  
  message("caching data ")  
  Inv
}
