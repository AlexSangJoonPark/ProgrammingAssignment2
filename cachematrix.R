## Coursera - R Programming
## Programming Assignment 2
## SangJoon Park

## Two functions provide caching feature of inverse of a matrix.
## cacheSolve function caches the inversed matrix after the first execution. 

## makeCacheMatrix is a function to return list object which is containing four functions.
##
## set : set an inversable matrix
## get : get the value of the inversable matrix
## setinversematrix : set the value of the inversed matrix
## getinversematrix : get the value of the inversed matrix
## 
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL                              # init variable of inversed matrix
    
    set <- function(y) {                    
        x <<- y                             # set a matrix of 'x'
        im <<- NULL                         # clear a inversed matrix
    }
    get <- function() x
    setinversematrix <- function(m) im <<- m # set a inversed matrix for caching
    getinversematrix <- function() im       # get a cached inversed matrix
    
    list(set = set, get = get,              # return list of four functions
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## cacheSolve is a function to inverse a matrix with caching.
## To cache the value, makeCacheMatrix needs to be used as an argument.
## From second call, cached value is returned with a message.
##
cacheSolve <- function(x, ...) {
    
    im <- x$getinversematrix()              # get cached inversed matrix
    if(!is.null(im)) {                      # if the cached matrix exists
        message("getting cached matrix")
        return(im)                          # return the cached matrix
    }
    
    m <- x$get()                            # get a matrix
    im <- solve(m, ...)                     # inverse the matrix
    x$setinversematrix(im)                  # set the inversed matrix for caching
    im                                      # return the inversed matrix of 'x'
}


