## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversedM <- NULL
  set <- function(y) {
    x <<- y
    inversedM <<- NULL
  }
  get <- function() x
  setInversedM <- function(calculatedInversedM) inversedM <<- calculatedInversedM
  getInversedM <- function() inversedM
  list(set = set, get = get , setInversedM = setInversedM ,getInversedM = getInversedM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversedM <- x$getInversedM()
  if(!is.null(inversedM) && identical(inversedM,solve(x$get()))){
    message("getting cached data")
    return(inversedM)
  }
  data <- x$get()
  inversedM <-  solve(data)
  x$setInversedM(inversedM)
  inversedM
}
