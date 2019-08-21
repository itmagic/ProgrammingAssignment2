
## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.

makeCacheMatrix <- function(x = matrix()) {
  set <- function(matrix = matrix()){
    x <<- matrix 
    inverse <<- NULL
           
  get <- function() x
  
  setInverse <- function(inverseMatrix = matrix()){ 
    inverse <<- inverseMatrix
  } 
  getInverse <- function() inverse
  list(get = get, set = set, getI = getInverse, setI = setInverse)
}

## checks if the given makeCacheMatrix object already has it's inverse calculate.
## If not, it calculates it's inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
  if(is.null(x$getI())){
    print("Competing inverse for the first time...") 
    x$setI(solve(x$get()))
  }else {print("Getting cached data")}
  
  x$getI()
}
