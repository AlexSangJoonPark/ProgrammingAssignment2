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
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversematrix <- function(m) im <<- m
    getinversematrix <- function() im
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## cacheSolve is a function to inverse a matrix with caching.
## To cache the value, makeCacheMatrix needs to be used as an argument.
## From second call, cached value is returned with a message.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinversematrix()
    if(!is.null(im)) {
        message("getting cached inversed matrix")
        return(im)
    }
    m <- x$get()
    im <- solve(m, ...)
    x$setinversematrix(im)
    im
}


