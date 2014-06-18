## makeCacheMatrix() is a function which returns a list of 4 functions to get and set the input
##                    and output matrix.
##
## cacheSolve() is a function which takes the list of functions created in makeCacheMatrix,
##               and checks in it to see if there is a cached version of the inverse 
##              available. if there is it gets it. If there is not, it creates one. 
##
## Demonstration:
## v <- makeCacheMatrix()   #intialize the first function which creates a list of 4 functions
## v$set(rbind(c(1, -1/4), c(-1/4, 1)))    #use the contained set function to create the input matrix
## v$get()   #use the contained get function to retreive the input
## cacheSolve(v)  #use your second function to check if there is a cached output matrix, and if not to create one
## cacheSolve(v)  #do the same again (this will deliver the cached version created above)


## makeCacheMatrix returns a list of 4 functions to get and set both the input and output matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
   x <<- y
   m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function which takes in the list of functions created in makeCacheMatrix,
##               and checks to see if there is a cached version of the inverse matrix
##              set. if there is it gets it. If there is not, it creates one. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
