## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##define the cache matrix m  
  m <- NULL 
  set <- function(y) {
    x <<- y ## assign the input matrix y to x
    m <<- NULL ##Re-initializing m to NULL
  }
  get <- function() x ##getting the matrix x
  setinverse <- function(inverse) m <<- inverse ## the inverse gets cached as m
  getinverse <- function() m ## getting the cached inverse of x 
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
  
}



##The following function calculates the mean of the special "matrix" 
##created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and 
##sets the value of the inverse of the special matrix in the cache via the 
##setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## get the inverse of x
  if(!is.null(m)) { ## checking whether it has been calculated and cached if calculated no need to calculate it again
    message("getting cached data") 
    return(m) # just return m
  }
  data <- x$get() ## otherwise get the data from x 
  m <- solve(data, ...) ## and calculate the inverse 
  x$setinverse(m) ## setting the inverse 
  m ## return the inverse
}
