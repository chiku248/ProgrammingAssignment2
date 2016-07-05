+# The following are two functions that involve matrices. 
 +# The first function stores the matrix in a cache allowing it to be retrieved 
 +# at a later point in another function. 
 +# The second function uses the matrix stored in the first function to find it's 
 +# inverse. 
  
 -## Write a short comment describing this function
 +# In order to use these functions, you first need to create a square matrix.
 +# Here are two examples of square matrices that I have created
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
-## Write a short comment describing this function
 +# This function will find the inverse of the matrix stored in makeCacheMatrix 
 +# function. 
 +# The input for cacheSolve has to be makeCacheMatrix or a value stored of 
 +# makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
