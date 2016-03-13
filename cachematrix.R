## Matrix Inversion  >>  Cache Results
## Two functions
## makeCacheMatix - which creates a list of 4 functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
## cacheSolve  which returns the inverse of a matrix either via
# computation or returns a cached answer if it exists

## makeCahcheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Using cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

##Test
## > x = rbind(c(3, -2), c(-1, 1))
## > a = makeCacheMatrix(x)
## > a$get()
##[,1] [,2]
##[1,]    3   -2
##[2,]   -1    1


##  First run so not cached
## > cacheSolve(a)
##[,1] [,2]
##[1,]    1    2
##[2,]    1    3

##  Subsequent run should use cached and display message
## > cacheSolve(a)
##Using cached data.
##[,1] [,2]
##[1,]    1    2
##[2,]    1    3