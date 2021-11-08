## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #to initialize inv as NULL which will hold value of inverse matrix 
  set <- function(y) {  #to define the set function to assign new 
    x <<- y             #value of matrix in parent environment
    inv <<- NULL        #if there is a new matrix it will reset inv to NULL
}
  get <- function() {x}   #define the get function - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" above
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    ## Return a matrix that is the inverse of 'x'
}
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

#an example to test the function above
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
cacheSolve(my_Matrix)