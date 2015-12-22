## Put comments here that give an overall description of what your
## functions do
##Assignement was a test in storing values in a cache. this will improve
##performance of the program if the values have to be reused.
## We learnt of a new operator <<- to cache the value in memory

##To test this, create a sqaure matrix for e.g. and call the fucntions in order:
## >m3 <- matrix(c(4,2,7,6), 2, 2)
## >sample <- makeCacheMatrix(m3)
## >m3Inv <- cacheSolve(sample)

##to test it actually worked, multiply both and you should get an identity matrix
## >m3 %*% m3Inv

##If you run the follwoing command again, it should print "Returning cached value
## >m3Inv <- cacheSolve(sample) 


## Write a short comment describing this function
##This function is an API that stores the values in cache.
##It has a pair of get/set methods that return the stored or default 
##inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##Default value is set to null
  solveMat <- NULL
  
  ## A pair of set and get functionf for accessing other fucntions
  set <- function(y) {
    x <<- y
    solveMat <<- NULL
  }
  get <- function() x
  
  ## A pair of set and get for access to inverse matrix
  setInverse <- function(inv) solveMat <<- inv
  getInverse <- function() solveMat
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function returns an inverse of a matrix.
##The argument x for this should be the output of 
##the makeCacheMatrix(<your matrix>) function call.

##this function first check to see if the inverse of a matrix
##is stored in memory. If so, it will return that value.
##If not it will calculate inverse and store it for future

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    ##return cached value
    message("Returning cached value")
    return(s)
  }
  
  ##There is no cached value, create an inverse and store it
  data <- x$get()
  s <- solve(data)
  x$setInverse(s)
  s
}
