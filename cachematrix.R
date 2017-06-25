## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{

  inv <- NULL
  setmatrix <- function(y) {
    #sets the matrix value  and the inverse value to null
    x <<- y
    inv <<- NULL
  }
  #returns the matrix 
  getmatrix <- function() x
  
  #set the inverse value of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #retuens the inverse value
  getinverse <- function()inv 
  
  #returns list of functions declared and sets the function.
  list(set = setmatrix, get = getmatrix,
       setinv= setinverse,
       getinv = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #checking if the value is null or not 
  if(!is.null(inv)) 
  {
    #if not null getting the result from the cached data
    message("getting cached data")
    return(inv)
  }
  #if null getting the matrix 
  datmatrix  <- x$get()
  
  #creating an inverse matrix 
  inv <- solve(datmatrix)
  
  #setting the value of the inversed matrix 
  x$setinv(inv)
  
  inv
}
