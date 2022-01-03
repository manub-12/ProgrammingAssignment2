## These are two functions that can cache the inverse of a matrix
## The first function creates a matrix that can cache its inverse
## The second function computes the inverse of the matrix

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set<- function(z) 
  {
    x <<- z
    i <<- NULL
  }
  get <- function()
  {
    return(x)
  } 
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function()
  {
    return(i)
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix 
## created in the function above (makeCacheMatrix)

cacheSolve <- function(x, ...) 
{
  i <- x$getInverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  return(i)
}



