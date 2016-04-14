## These two functions calculate the inverse of a square matrix, differing
## to a cached value of the invers whenever possible.


## This function does 4 things
## set - creates a cached copy of the matrix
## get - returns the  matrix
## setInverse - sets the cached value of the matrix's inverse
## setInverse - returns the cached value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { # set the cached value of the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x # get the cached value of the matrix
  setInverse <- function(inv) inverse <<- inv # set the cached value of the inverse
  getInverse <- function() inverse # get the cached value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function 
## 1- checks to see if an inverse of the matrix is cached
## 2- returns the cached copy if available
## 3- calculates the inverse if not available
## 4- returns the calculated or cached inverse

cacheSolve <- function(x, ...) {
  inverse=x$getInverse() # check to see if there is a cached or NULL value of the inverse
  
  if(!is.null(inverse)){  # if there is a cached inverse
    message("getting cached data")
    return(inverse) ## Return the cached value
  }
  
  y<-solve(x$get()) # other wise, calculate it
  x$setInverse(y) # and set a cached value
  
  return(y) ## return the newly cached value of the inverse
}

A<-matrix(c(1:4),2,2)
B<-makeCacheMatrix(A)
cacheSolve(B)
