## cachematrix.R contains two functions makeCacheMatrix and cacheSolve that enable a programmer 
## to create a special matrix object that stores the matrix and its inverse in memory and further 
## to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.


## makeCacheMatrix creates a special "matrix", which is a list containing 4 functions to 1) set the value
## of the matrix, 2) get the value of the matrix, 3) set the value of the inverse, 4) get the value of the inverse
## Input: matrix (A)
## Example: myMatrix<-makeCacheMatrix(A) , where A is a matrix
## Output: object (myMatrix), containing 4 functions set(),get(), setinverse() and getinverse() and two data 
## objects x and inv

makeCacheMatrix <- function(x = matrix()) { # initialization of x as an argument of the function, by default empty numeric vector
  inv <- NULL                               # initialization of object inv
  ## Define the set() function
  set <- function(y) {
    x <<- y                               # assign the input argument to the x in the parent environment
    inv <<- NULL                          # assign the value of NULL to the inv object in the parent environment to clear the cache
  }
  get <- function() x                     # retrieves x it from the parent environment of makeCacheMatrix
  setinverse <- function(solve) inv <<- solve #assign the input argument solve to the value of inv in the parent environment
  getinverse <- function() inv            # # retrieves inv
  ## assign each of the above functions as an element within a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see if the mean 
## has already been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via 
## the setmean function.


## Description: cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## Input: special "matrix" (for example myMatrix) of type makeCacheMatrix
## Output: inverse of the matrix
## Algorithm: 1) cacheSolve checks whether the inverse was calculated before  cacheSolve, 2) if 
## the inverse has already been calculated  cacheSolve retrieves the inverse from the cache,
## 3) if the inverse was not calculated before it calculates the inverse from the data

cacheSolve <- function(x, ...) {
  ## retrieve the inverse     
  inv <- x$getinverse()
  ## check if the inverse hasn't been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)                 ## if the inverse was calculated return it to the parent environment
  }
  data <- x$get()               ## get the matrix from the input object
  inv <- solve(data, ...)       ## calculates the inverse from the data
  x$setinverse(inv)             ##  set the inverse in the input object
  ## Return a matrix that is the inverse of 'x'
  inv
}
