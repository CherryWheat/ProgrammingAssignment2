## These functions create a method for cacheing the inverse of a matrix in a 
## bizzare list structure for some strange reason

## This function creates a weird list to augment a matrix with
## a cached version of its own inverse and functions to modify it

makeCacheMatrix <- function(x = matrix()) {
  
## Set inverse to NULL
  
  inv <- NULL
  
## Create function to overwrite matrix value and reset cached inverse value
  
  set <- function(mValue){
    x <<- mValue
    inv <<- NULL
  }
  
## Create function to return matrix value
  
  get <- function() x
  
## Create function to cache inverse value
  
  setinverse <- function(inv_value) inv<<- inv_value
  
## Create function to get inverse value
  
  getinverse <- function() inv
  
## Return list version of matrix with cached inverse - note: this is a ridiculous way to do this
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves a cached inverse value if it exists
## and caculates and caches it if it doesn't

cacheSolve <- function(x, ...) {
  
## Try to get the cached inverse value and return it if it exists
  
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  
## Otherwise calculate, cache and return the inverse
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
