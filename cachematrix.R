## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## the function can be used to create a special object
## containing a matrix and caching its inverse.
## The methods allow to read and write the matrix and its 
## inverse for this object.


makeCacheMatrix <- function(x = matrix()) {

  ## initialize the inverse of the matrix to NULL
  inv <- NULL
  
  ## set the matrix to y, and reset the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## set the inverse of the matrix to a_inv
  setinv <- function(a_inv) inv <<- a_inv
  
  ## get the inverse of the matrix
  getinv <- function() inv
  
  ## return the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function:
## the function accepts as input an object created with
## makeCacheMatrix and return the inverse of the matrix
## associated to the input. 
## If the inverse of the matrix is not NULL then the function 
## returns the inverse already cached. Otherwise, it computes the 
## inverse and it returns it, and it updates it in the input object.

cacheSolve <- function(x, ...) {
  
  ## get the inverse of the matrix stored in the object x
  inv <- x$getinv()
  
  ## If the inverse is not NULL return the inverse cached 
  ## which was read using getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## here the inverse was NULL, so it must be computed using solve()
  data <- x$get()
  inv <- solve(data, ...)
  
  ## update the inverse in x
  x$setinv(inv)
  
  ##return the inverse
  inv
  
}
