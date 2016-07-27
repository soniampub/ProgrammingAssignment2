## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creats a matrix object that is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # set inv of matrix x to NULL
  # inv is the cached value of x
  inv <- NULL
  
  # set a new value to matrix
  set <- function(new_x){
    x <<- new_x
    inv <<- NULL
  }
  
  # get the matrix
  get <- function(){x}
  
  # set cache inv of a matrix
  setInverse <- function(cache_inv){
    inv <<- cache_inv
  }
  
  # get the inverse
  getInverse <- function(){inv}
  
  # return all these func as list
  list (set=set, get=get, setInvers=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

# This function calculate invers of matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #get the cached inverse
  inv <- x$getInservse()
  
  #check if cache inverse is there
  if(!is.null(inv)){
    message("getting data from cache")
    return(inv)
  }
  
  # calculate the inverse and cache it
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
