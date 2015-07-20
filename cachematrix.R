## Put comments here that give an overall description of what your
## functions do

# This file contains two functions.
# The first function (makeCacheMatrix) creates objects that can contain both a 
# (square) matrix and its (cached) inverse. 
# The second will display the inverse of the matrix that is cached in the object.
# If no cached matrix is present, it will calculate it and store it in the object.

# The first function creates objects that can contain both a 
# (square) matrix and its (cached) inverse. 
# Technically the returned object is a list of four functions. 
# - The first function returns the matrix with which the function was called.
# - The second function assigns a (new) matrix to the object, overwriting the first
# - The third function assigns an inverse of the matrix to the object cache
# - The fourth function returns the cached inverse of the matrix, if there is any
makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x) || nrow(x)!=ncol(x)) {
        message('input is not a (square) matrix!\nThis function was made for matrices for which an inverse exists')
        return()
    }
    inv <- NULL    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }    
    get <- function() x
    setinverse <- function(inverse = matrix()) inv <<- inverse
    getinverse <- function() inv    
    list(get = get, set = set,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# this function will display the inverse of the matrix that is cached in the object.
# If no cached inverse matrix is present, it will calculate it and store it in the object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message('reading inverse matrix from cache:')
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv  
}
