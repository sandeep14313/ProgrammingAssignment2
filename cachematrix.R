## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  # cached inverse of matrix
  inv <- NULL  
  ## get and set for matrix
  get <- function() x
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  ## get and set for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if (!is.null(inv)) 
  {
    message("inverse is cached")
    #returns cached matrix inverse if already computed
    return(inv)
  }
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  # cache inverse
  x$setinv(inv)  
  # returns the inverse of the matrix
  return(inv)
}
