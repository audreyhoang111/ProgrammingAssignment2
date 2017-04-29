## This function will create a matrix object that is a list of 4 mini-functions 
## used to set the matrix, get/return the matrix, set the inverse, get/return the inverse

## create a matrix object used for caching

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      
      set_inverse <- function(solve) i <<- solve
      
      get_inverse <- function() i
      
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## return cached inverse if existed, otherwise calculate new inverse using solve()

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$get_inverse()
      
      if(!is.null(i)){
            message("getting cached data hola!")
            return(i)
      }
      
      data <- x$get()
      i <- solve(data,...)
      x$set_inverse(i)
      i
}


