## Matrix inversion calculation mdifications by 
## 7/31/2016 by Juan Villarreal
## The intention is to calculate matrix inversion, using interesting fetures of R like Catch values in factors using "<<" operator

## MakeCacheMatrix,can receive a square matrix to return the inverse matrix. 
## use set option to introduce a new square matrix
## use get option to display the current square matrix in the variable x

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) i <<- solve
      getsolve <- function() i
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve is intented to validate if the matrix inverse "i" have values and if it also have been calculated, 
## if "i" is not null, it returns last inverse matrix values.
## if "i" is null, the function calculate the inverse result of "x" matrix and display the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getsolve()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setsolve(i)
      i
}
